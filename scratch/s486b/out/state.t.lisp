;;; pcl: pipeline=v2 gen=v2-1460
(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/pl2cl")
(pcl::box-set pcl::$0 "state.t")
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

(pcl:p-defpackage :countfetches)

(p-declare-sub pl-stateful)
(p-declare-sub pl-nesting)
(p-declare-sub pl-generator)
(p-declare-sub pl-gen_cashier)
(p-declare-sub pl-stateless)
(p-declare-sub pl-stateful_array)
(p-declare-sub pl-stateful_init_array)
(p-declare-sub pl-stateful_hash)
(p-declare-sub pl-stateful_init_hash)
(p-declare-sub pl-noseworth)
(p-declare-sub pl-pugnax)
(p-declare-sub pl-president)
(p-declare-sub pl-reference)
(p-declare-sub pl-rt_123029)
(p-declare-sub countfetches::pl-TIESCALAR)
(p-declare-sub countfetches::pl-FETCH)
(p-declare-sub countfetches::pl-foo)
(p-declare-sub pl-thing)
(p-declare-sub pl-thing2)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $amount (make-p-box nil))
(p-defcell $c (make-p-box nil))
(p-defcell $c__state__8 (make-p-box nil) :perl "$c" :why :state-cell)
(p-defcell $c__state__8__init (make-p-box nil))
(p-defcell $calvin__state__16 (make-p-box nil) :perl "$calvin" :why :state-cell)
(p-defcell $calvin__state__16__init (make-p-box nil))
(p-defcell $d (make-p-box nil))
(p-defcell $e (make-p-box nil))
(p-defcell $expect (make-p-box nil))
(p-defcell $f (make-p-box nil))
(p-defcell $f1 (make-p-box nil))
(p-defcell $f2 (make-p-box nil))
(p-defcell $f__state__1 (make-p-box nil) :perl "$f" :why :state-cell)
(p-defcell $f__state__1__init (make-p-box nil))
(p-defcell $flower__state__14 (make-p-box nil) :perl "$flower" :why :state-cell)
(p-defcell $flower__state__14__init (make-p-box nil))
(p-defcell $level (make-p-box nil))
(p-defcell $next (make-p-box nil))
(p-defcell $outer (make-p-box nil))
(p-defcell $president_answer (make-p-box nil))
(p-defcell $ref1 (make-p-box nil))
(p-defcell $ref2 (make-p-box nil))
(p-defcell $s__state__0 (make-p-box nil) :perl "$s" :why :state-cell)
(p-defcell $s__state__0__init (make-p-box nil))
(p-defcell $s__state__2 (make-p-box nil) :perl "$s" :why :state-cell)
(p-defcell $s__state__2__init (make-p-box nil))
(p-defcell $simpson__state__17 (make-p-box nil) :perl "$simpson" :why :state-cell)
(p-defcell $simpson__state__17__init (make-p-box nil))
(p-defcell $spam (make-p-box nil))
(p-defcell $t (make-p-box nil))
(p-defcell $tb__state__7 (make-p-box nil) :perl "$tb" :why :state-cell)
(p-defcell $tb__state__7__init (make-p-box nil))
(p-defcell $tintin (make-p-box nil))
(p-defcell $uninit__state__25 (make-p-box nil) :perl "$uninit" :why :state-cell)
(p-defcell $vi (make-p-box nil))
(p-defcell $vile__state__15 (make-p-box nil) :perl "$vile" :why :state-cell)
(p-defcell $vile__state__15__init (make-p-box nil))
(p-defcell $x (make-p-box nil))
(p-defcell $x__state__11 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__12 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__12__init (make-p-box nil))
(p-defcell $x__state__13 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__13__init (make-p-box nil))
(p-defcell $x__state__24 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__3 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__4 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__4__init (make-p-box nil))
(p-defcell $x__state__5 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__5__init (make-p-box nil))
(p-defcell $xhval (make-p-box nil))
(p-defcell $xsize (make-p-box nil))
(p-defcell $y (make-p-box nil))
(p-defcell $y__state__10 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__19 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__19__init (make-p-box nil))
(p-defcell $y__state__20 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__20__init (make-p-box nil))
(p-defcell $y__state__21 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__21__init (make-p-box nil))
(p-defcell $y__state__22 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__22__init (make-p-box nil))
(p-defcell $y__state__23 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__23__init (make-p-box nil))
(p-defcell $z (make-p-box nil))
(p-defcell $z__state__18 (make-p-box nil) :perl "$z" :why :state-cell)
(p-defcell $z__state__18__init (make-p-box nil))
(p-defcell $z__state__9 (make-p-box nil) :perl "$z" :why :state-cell)
(p-defcell @apollo (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @f (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @flowers (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @forbidden_items (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @result1 (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @result2 (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @simpsons (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @spam (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @thunderbirds (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @thunderbirds2 (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @warnings (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell countfetches::$fetchcount (make-p-box nil))

(p-defcell $x__state__26 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $y__state__27 (make-p-box nil) :perl "$y" :why :state-cell)
(p-defcell $y__state__27__init nil)
(p-defcell $t__state__28 (make-p-box nil) :perl "$t" :why :state-cell)
(p-defcell $foo__state__29 (make-p-box nil) :perl "$foo" :why :state-cell)
(p-defcell $foo__state__29__init nil)
(p-defcell $bar__state__30 (make-p-box nil) :perl "$bar" :why :state-cell)
(p-defcell $bar__state__30__init nil)
(p-defcell $cash_in_store__state__32 (make-p-box nil) :perl "$cash_in_store" :why :state-cell)
(p-defcell $cash_in_store__state__32__init nil)
(p-defcell $reinitme__state__33 (make-p-box nil) :perl "$reinitme" :why :state-cell)
(p-defcell $reinitme__state__33__init nil)
(p-defcell @x__state__34 (make-array 0 :adjustable t :fill-pointer 0) :perl "@x" :why :state-cell)
(p-defcell @x__state__35 (make-array 0 :adjustable t :fill-pointer 0) :perl "@x" :why :state-cell)
(p-defcell @x__state__35__init nil)
(p-defcell %hx__state__36 (make-hash-table :test 'equal) :perl "%hx" :why :state-cell)
(p-defcell %x__state__37 (make-hash-table :test 'equal) :perl "%x" :why :state-cell)
(p-defcell %x__state__37__init nil)
(p-defcell $recursed_state__state__38 (make-p-box nil) :perl "$recursed_state" :why :state-cell)
(p-defcell $recursed_state__state__38__init nil)
(p-defcell $president__state__39 (make-p-box nil) :perl "$president" :why :state-cell)
(p-defcell $president__state__39__init nil)
(p-defcell $s__state__42 (make-p-box nil) :perl "$s" :why :state-cell)
(defvar countfetches::$a (make-p-box nil))
(defvar countfetches::$b (make-p-box nil))
(p-defcell countfetches::$fetchcount (make-p-box nil))
(p-defcell $y__file__0 (make-p-box nil) :perl "$y" :why :captured)
(p-defcell $x__state__31 (make-p-box nil) :perl "$x" :why :state-cell)
(p-defcell $x__state__31__init nil)
(p-defcell @presidents (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell $z__state__40 (make-p-box nil) :perl "$z" :why :state-cell)
(p-defcell $z__state__41 (make-p-box nil) :perl "$z" :why :state-cell)
;; PPI::Statement::Data â register DATA filehandle
(p-install-data-handle 'DATA (p-esc "(state $a) = 1;\\n(state @a) = 1;\\n(state @a :shared) = 1;\\n(state %a) = ();\\n(state %a :shared) = ();\\nstate ($a) = 1;\\n(state ($a)) = 1;\\nstate (@a) = 1;\\n(state (@a)) = 1;\\nstate (@a) :shared = 1;\\n(state (@a) :shared) = 1;\\nstate (%a) = ();\\n(state (%a)) = ();\\nstate (%a) :shared = ();\\n(state (%a) :shared) = ();\\nstate (undef, $a) = ();\\n(state (undef, $a)) = ();\\nstate (undef, @a) = ();\\n(state (undef, @a)) = ();\\nstate ($a, undef) = ();\\n(state ($a, undef)) = ();\\nstate ($a, $b) = ();\\n(state ($a, $b)) = ();\\nstate ($a, $b) :shared = ();\\n(state ($a, $b) :shared) = ();\\nstate ($a, @b) = ();\\n(state ($a, @b)) = ();\\nstate ($a, @b) :shared = ();\\n(state ($a, @b) :shared) = ();\\nstate (@a, undef) = ();\\n(state (@a, undef)) = ();\\nstate (@a, $b) = ();\\n(state (@a, $b)) = ();\\nstate (@a, $b) :shared = ();\\n(state (@a, $b) :shared) = ();\\nstate (@a, @b) = ();\\n(state (@a, @b)) = ();\\nstate (@a, @b) :shared = ();\\n(state (@a, @b) :shared) = ();\\n(state $a, state $b) = ();\\n(state $a, $b) = ();\\n(state $a, my $b) = ();\\n(state $a, state @b) = ();\\n(state $a, local @b) = ();\\n(state $a, undef, state $b) = ();\\nstate ($a, undef, $b) = ();\\n"))

;; BEGIN {

(p-BEGIN

  (p-set-current-package :main "main")

  ;; chdir 't' if -d 't'

  (p-if (p--d "t") (p-chdir "t"))

  ;; require './test.pl'

  (p-eval-always

    (p-require-file "./test.pl"))

  ;; set_up_inc('../lib')

  (p-set_up_inc "../lib")

)

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "feature"))

(p-sub pl-stateful
  (&rest %_args)
  (:writes-args nil :needs (:nonlocal_exit.return))
  (p-args-body
    (block nil
      (p-void-ctx $x__state__26
        (unless $y__state__27__init (box-set $y__state__27 1) (setf $y__state__27__init t))
        $y__state__27
        (p-let (($z :box (make-p-box nil)))
          (p-my-= $z 2)
          $t__state__28
          (p-//= $t__state__28 3)
          ;; return ($x__state__26++, $y__state__27++, $z++, $t__state__28++)
(p-return (p-post++ $x__state__26) (p-post++ $y__state__27) (p-post++ $z) (p-post++ $t__state__28)))))))

(p-sub pl-nesting
  (&rest %_args)
  (:writes-args nil :needs (:nonlocal_exit.return))
  (p-args-body
    (block nil
      (p-void-ctx
        (unless $foo__state__29__init
          (box-set $foo__state__29 10)
          (setf $foo__state__29__init t))
        $foo__state__29
        (p-let (($t :box (make-p-box nil)))
          (let ((*package* *package*))
            (block nil
              (tagbody :redo
                (unless $bar__state__30__init
                  (box-set $bar__state__30 12)
                  (setf $bar__state__30__init t))
                $bar__state__30
                (p-my-= $t (p-pre++ $bar__state__30))
                :next)))
          (p-pre++ $foo__state__29)
          ;; return ($foo__state__29, $t)
(p-return $foo__state__29 $t))))))

(p-sub pl-generator
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx
        (p-let (($outer :box (make-p-box nil) :captured t))
          (p-caller-ctx
            (funcall
              (lambda ()
                (progn
                  (p-let
                    (($x__state__24 :box
                        (make-p-box nil)
                        :perl "$x" :why :state-cell :captured t))
                    (lambda (&rest %_args)
                      (let
                        ((@_ (p-flatten-args %_args))
                          (*pcl-current-package* "main")
                          (*pcl-caller-wantarray* *wantarray*))
                        (p-sub-frame
                          (block nil
                            (p-void-ctx (p-pre++ $outer)
                              (p-caller-ctx (p-pre++ $x__state__24)))))))))))))))))

(p-sub countfetches::pl-TIESCALAR
  (&rest %_args)
  (:needs ())
  (p-args-body (block nil (p-bless (make-p-box (p-hash)) "countfetches"))))

(p-sub countfetches::pl-FETCH
  (&rest %_args)
  (:needs ())
  (p-args-body (block nil (p-void-ctx (p-pre++ countfetches::$fetchcount) (p-caller-ctx 18)))))

(p-sub countfetches::pl-foo
  (&rest %_args)
  (:captures ($y__file__0) :needs ())
  (p-args-body
    (block nil
      (p-void-ctx
        (unless $x__state__31__init
          (box-set $x__state__31 $y__file__0)
          (setf $x__state__31__init t))
        $x__state__31
        (p-caller-ctx (p-post++ $x__state__31))))))

(p-sub pl-gen_cashier
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-raw-params (($amount :scalar :captured t))
    (block nil
      (p-void-ctx
        (unless $cash_in_store__state__32__init
          (box-set $cash_in_store__state__32 0)
          (setf $cash_in_store__state__32__init t))
        $cash_in_store__state__32
        (p-caller-ctx
          (p-tail-value
            (make-p-box
              (p-hash "add"
                (lambda (&rest %_args)
                  (let
                    ((@_ (p-flatten-args %_args))
                      (*pcl-current-package* "main")
                      (*pcl-caller-wantarray* *wantarray*))
                    (p-sub-frame (block nil (p-incf $cash_in_store__state__32 $amount)))))
                "del"
                (lambda (&rest %_args)
                  (let
                    ((@_ (p-flatten-args %_args))
                      (*pcl-current-package* "main")
                      (*pcl-caller-wantarray* *wantarray*))
                    (p-sub-frame (block nil (p-decf $cash_in_store__state__32 $amount)))))
                "bal"
                (lambda (&rest %_args)
                  (let
                    ((@_ (p-flatten-args %_args))
                      (*pcl-current-package* "main")
                      (*pcl-caller-wantarray* *wantarray*))
                    (p-sub-frame (block nil $cash_in_store__state__32))))))))))))

(p-sub pl-stateless
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx
        (unless $reinitme__state__33__init
          (box-set $reinitme__state__33 42)
          (setf $reinitme__state__33__init t))
        $reinitme__state__33
        (p-caller-ctx (p-pre++ $reinitme__state__33))))))

(p-sub pl-stateful_array
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx @x__state__34
        (p-push @x__state__34 "x")
        (p-caller-ctx (p-tail-value (p-array-last-index @x__state__34)))))))

(p-sub pl-stateful_init_array
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx
        (unless @x__state__35__init
          (p-array-= @x__state__35 (vector "a" "b" "c"))
          (setf @x__state__35__init t))
        @x__state__35
        (p-push @x__state__35 "x")
        (p-caller-ctx (p-tail-value (p-join "," @x__state__35)))))))

(p-sub pl-stateful_hash
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx %hx__state__36
        (p-caller-ctx (p-tail-value (p-post++ (p-gethash-box %hx__state__36 "foo"))))))))

(p-sub pl-stateful_init_hash
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx
        (unless %x__state__37__init
          (p-scalar-ctx (p-hash-= %x__state__37 (vector "a" "b" "c" "d")))
          (setf %x__state__37__init t))
        %x__state__37
        (p-post++ (p-gethash-box %x__state__37 "foo"))
        (p-caller-ctx
          (p-tail-value
            (p-join ","
              (p-list-ctx
                (p-map (lambda ($_) (p-flatten-args (list $_ (p-gethash %x__state__37 $_))))
                  (p-list-ctx (%p-sort-classic :default (p-list-ctx (p-keys %x__state__37)))))))))))))

(p-sub pl-noseworth
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-raw-params (($level :scalar))
    (block nil
      (p-void-ctx
        (unless $recursed_state__state__38__init
          (box-set $recursed_state__state__38 123)
          (setf $recursed_state__state__38__init t))
        $recursed_state__state__38
        (pl-is $recursed_state__state__38
          123
          (p-string-concat "state kept through recursion (" $level ")"))
        (p-caller-ctx
          (let ((--pcl-if-ret--0 nil))
            (p-if (setf --pcl-if-ret--0 $level)
              (setf --pcl-if-ret--0 (pl-noseworth (p-- $level 1)))
              nil)
            --pcl-if-ret--0))))))

(p-sub pl-pugnax
  (&rest %_args)
  (:wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx
        (p-let (($x :box (make-p-box nil)))
          (p-my-= $x
            (p-scalar-ctx
              (funcall
                (lambda ()
                  (progn
                    (p-if (p-! $y__state__23__init)
                      (progn (p-scalar-= $y__state__23 42) (p-scalar-= $y__state__23__init 1)))
                    $y__state__23)))))
          (p-post++ $y__state__23)
          (p-caller-ctx $x))))))

(p-sub pl-president
  (&rest %_args)
  (:writes-args t :captures (@presidents) :needs (:nonlocal_exit.goto))
  (p-args-body
    (block nil
      (p-void-ctx
        (p-let (($next :box (make-p-box nil)))
          (p-my-= $next (p-shift @presidents))
          (unless $president__state__39__init
            (box-set $president__state__39 $next)
            (setf $president__state__39__init t))
          $president__state__39
          (p-if @presidents (p-goto-sub #'pl-president))
          (p-caller-ctx $president__state__39))))))

(p-sub pl-reference
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-backslash $x__state__11))))

(p-eval-always (p-note-inc "warnings"))

(p-eval-always (p-note-inc "warnings"))

(p-eval-always (p-note-inc "strict"))

(p-sub pl-thing
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-raw-params (($expect :scalar))
    (block nil
      (p-void-ctx
        (p-let (($x :box (make-p-box nil)) ($y :box (make-p-box nil)))
          $z__state__40
          (pl-is $z__state__40 $expect "State variable is correct")
          (p-caller-ctx (p-scalar-= $z__state__40 5)))))))

(p-sub pl-thing2
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-raw-params (($expect :scalar))
    (block nil
      (p-void-ctx
        (p-let (($x :scalar (p-undef)))
          (p-let (($y :scalar (p-undef)))
            $z__state__41
            (pl-is $z__state__41 $expect "State variable is correct")
            (p-caller-ctx (p-scalar-= $z__state__41 6))))))))

(p-sub pl-rt_123029
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil
      (p-void-ctx $s__state__42
        (p-scalar-= $s__state__42 (p-str-x "foo" 500))
        (p-let (($c :box (make-p-box nil)))
          (p-my-= $c $s__state__42)
          (p-caller-ctx (p-tail-value (p-defined $s__state__42))))))))

(p-run-compile-phase-blocks)

(p-void-ctx (pl-plan "tests" 166))

(p-void-ctx
  (pl-ok (p-scalar (p-eval "CORE::state $x = 1;")) "CORE::state outside of feature.pm scope"))

(p-void-ctx
  (pl-ok (p-scalar (p-! (p-defined $uninit__state__25))) "state vars are undef by default"))

(locally (declare (notinline pcl::p-+ pcl::p-- pcl::p-* pcl::p-/ pcl::p-% pcl::p-== pcl::p-!= pcl::p-< pcl::p-> pcl::p-<= pcl::p->= pcl::p-<=> pcl::p-. pcl::p-str-eq pcl::p-str-ne pcl::p-str-lt pcl::p-str-gt pcl::p-str-le pcl::p-str-ge pcl::p-str-cmp pcl::unbox pcl::to-number pcl::to-string pcl::p-true-p pcl::p-bool pcl::%pcl-nan-p))
(p-let
  (($x :box (make-p-box nil) :captured t)
    ($y :box (make-p-box nil))
    ($z :box (make-p-box nil))
    ($t :box (make-p-box nil)))
  (p-scalar-ctx (p-list-= (vector $x $y $z $t) (p-list-ctx (pl-stateful))))
  (p-void-ctx (pl-is $x 0 "uninitialized state var"))
  (p-void-ctx (pl-is $y 1 "initialized state var"))
  (p-void-ctx (pl-is $z 2 "lexical"))
  (p-void-ctx (pl-is $t 3 "initialized state var, list syntax"))
  (p-list-= (vector $x $y $z $t) (p-list-ctx (pl-stateful)))
  (p-void-ctx (pl-is $x 1 "incremented state var"))
  (p-void-ctx (pl-is $y 2 "incremented state var"))
  (p-void-ctx (pl-is $z 2 "reinitialized lexical"))
  (p-void-ctx (pl-is $t 4 "incremented state var, list syntax"))
  (p-list-= (vector $x $y $z $t) (p-list-ctx (pl-stateful)))
  (p-void-ctx (pl-is $x 2 "incremented state var"))
  (p-void-ctx (pl-is $y 3 "incremented state var"))
  (p-void-ctx (pl-is $z 2 "reinitialized lexical"))
  (p-void-ctx (pl-is $t 5 "incremented state var, list syntax"))
  (p-list-= (vector $x $y) (p-list-ctx (pl-nesting)))
  (p-void-ctx (pl-is $x 11 "outer state var"))
  (p-void-ctx (pl-is $y 13 "inner state var"))
  (p-list-= (vector $x $y) (p-list-ctx (pl-nesting)))
  (p-void-ctx (pl-is $x 12 "outer state var"))
  (p-void-ctx (pl-is $y 14 "inner state var"))
  (p-let (($f1 :box (make-p-box nil)))
    (p-my-= $f1 (p-scalar-ctx (pl-generator)))
    (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (p-funcall-ref $f1))) 1 "generator 1"))
    (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (p-funcall-ref $f1))) 2 "generator 1"))
    (p-let (($f2 :box (make-p-box nil)))
      (p-my-= $f2 (p-scalar-ctx (pl-generator)))
      (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (p-funcall-ref $f2))) 1 "generator 2"))
      (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (p-funcall-ref $f1))) 3 "generator 1 again"))
      (p-void-ctx
        (pl-is (p-scalar (p-scalar-ctx (p-funcall-ref $f2))) 2 "generator 2 once more"))
      (let ((*package* *package*))
        (block nil
          (tagbody :redo
            (p-defpackage :countfetches)
            (defclass countfetches::plc-countfetches () ())
            (p-set-current-package :countfetches "countfetches")
            (p-scalar-= countfetches::$fetchcount 0)
            (p-alias-eval-cell '$y $y__file__0)
            (p-tie $y__file__0 "countfetches")
            (p-void-ctx
              (main::pl-is (p-scalar (p-scalar-ctx (countfetches::pl-foo)))
                18
                "initialisation with tied variable"))
            (p-void-ctx
              (main::pl-is (p-scalar (p-scalar-ctx (countfetches::pl-foo)))
                19
                "increments correctly"))
            (p-void-ctx
              (main::pl-is (p-scalar (p-scalar-ctx (countfetches::pl-foo)))
                20
                "increments correctly, twice"))
            (p-void-ctx (main::pl-is countfetches::$fetchcount 1 "fetch only called once"))
            (p-set-current-package :main "main")
            :next)))
      (p-void-ctx (p-funcall-ref (p-gethash-deref (p-void-ctx (pl-gen_cashier 59)) "add")))
      (p-void-ctx (p-funcall-ref (p-gethash-deref (p-void-ctx (pl-gen_cashier 17)) "del")))
      (p-void-ctx
        (pl-is
          (p-scalar
            (p-scalar-ctx
              (p-funcall-ref (p-gethash-deref (p-scalar-ctx (pl-gen_cashier)) "bal"))))
          42
          "$42 in my drawer"))
      (p-void-ctx
        (pl-is (p-scalar (p-scalar-ctx (pl-stateless))) 43 "stateless function, first time"))
      (p-void-ctx
        (pl-is (p-scalar (p-scalar-ctx (pl-stateless))) 44 "stateless function, second time"))
      (p-let (($xsize :box (make-p-box nil)))
        (p-my-= $xsize (p-scalar-ctx (pl-stateful_array)))
        (p-void-ctx (pl-is $xsize 0 "uninitialized state array"))
        (p-my-= $xsize (p-scalar-ctx (pl-stateful_array)))
        (p-void-ctx (pl-is $xsize 1 "uninitialized state array after one iteration"))
        (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (pl-stateful_init_array))) "a,b,c,x"))
        (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (pl-stateful_init_array))) "a,b,c,x,x"))
        (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (pl-stateful_init_array))) "a,b,c,x,x,x"))
        (p-let (($xhval :box (make-p-box nil)))
          (p-my-= $xhval (p-scalar-ctx (pl-stateful_hash)))
          (p-void-ctx (pl-is $xhval 0 "uninitialized state hash"))
          (p-my-= $xhval (p-scalar-ctx (pl-stateful_hash)))
          (p-void-ctx (pl-is $xhval 1 "uninitialized state hash after one iteration"))
          (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (pl-stateful_init_hash))) "a,b,c,d,foo,1"))
          (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (pl-stateful_init_hash))) "a,b,c,d,foo,2"))
          (p-void-ctx (pl-is (p-scalar (p-scalar-ctx (pl-stateful_init_hash))) "a,b,c,d,foo,3"))
          (let ((*package* *package*))
            (p-dyn-once
              (block SKIP
                (catch (pcl::%pcl-loop-tag "LAST" 'SKIP)
                  (block nil
                    (catch (pcl::%pcl-loop-tag "NEXT" 'SKIP)
                      (tagbody :redo
                        (catch (pcl::%pcl-loop-tag "REDO" 'SKIP)
                          (progn
                            (p-if (p-scalar-ctx (pl-is_miniperl))
                              (p-void-ctx (pl-skip "no attributes in miniperl" 3)))
                            (p-eval
                              (p-esc
                                "\\nsub stateful_attr {\\n    state $a :shared;\\n    state $b :shared = 3;\\n    state @c :shared;\\n    state @d :shared = qw(a b c);\\n    state %e :shared;\\n    state %f :shared = qw(a b c d);\\n    $a++;\\n    $b++;\\n    push @c, \"x\";\\n    push @d, \"x\";\\n    $e{e}++;\\n    $f{e}++;\\n    return join(\",\", $a, $b, join(\":\", @c), join(\":\", @d), join(\":\", %e),\\n\\t    join(\":\", map { ($_, $f{$_}) } sort keys %f));\\n}\\n")
                              (list
                                (cons "$f1" $f1)
                                (cons "$f2" $f2)
                                (cons "$t" $t)
                                (cons "$x" $x)
                                (cons "$xhval" $xhval)
                                (cons "$xsize" $xsize)
                                (cons "$y" $y)
                                (cons "$z" $z)))
                            (p-void-ctx
                              (pl-is (p-scalar (p-scalar-ctx (pl-stateful_attr)))
                                "1,4,x,a:b:c:x,e:1,a:b:c:d:e:1"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-scalar-ctx (pl-stateful_attr)))
                                "2,5,x:x,a:b:c:x:x,e:2,a:b:c:d:e:2"))
                            (p-void-ctx
                              (pl-is (p-scalar (p-scalar-ctx (pl-stateful_attr)))
                                "3,6,x:x:x,a:b:c:x:x:x,e:3,a:b:c:d:e:3"))
                            (go :next)))
                        (go :redo)
                        :next)))))))
          (p-void-ctx (pl-noseworth 2))
          (p-void-ctx (pl-is (p-scalar (pl-pugnax)) 42 "scalar state assignment return value"))
          (p-void-ctx (pl-is (p-scalar (pl-pugnax)) 43 "scalar state assignment return value"))
          (p-foreach-range ($x 1 3)
            :my
            t
            (p-void-ctx
              (funcall
                (lambda ()
                  (progn
                    (p-if (p-! $y__state__22__init)
                      (progn (p-scalar-= $y__state__22 $x) (p-scalar-= $y__state__22__init 1)))
                    $y__state__22))))
            (p-void-ctx (pl-is $y__state__22 1 (p-string-concat "foreach " $x))))
          (p-let (($x :scalar 1))
            (p-for ()
              ((p-< $x 4))
              ((p-incf-raw $x :numeric))
              (p-void-ctx
                (funcall
                  (lambda ()
                    (progn
                      (p-if (p-! $y__state__21__init)
                        (progn (p-scalar-= $y__state__21 $x)
                          (p-scalar-= $y__state__21__init 1)))
                      $y__state__21))))
              (p-void-ctx (pl-is $y__state__21 1 (p-string-concat "for " $x)))))
          (p-while (p-< $x 4)
            (p-void-ctx
              (funcall
                (lambda ()
                  (progn
                    (p-if (p-! $y__state__20__init)
                      (progn (p-scalar-= $y__state__20 $x) (p-scalar-= $y__state__20__init 1)))
                    $y__state__20))))
            (p-void-ctx (pl-is $y__state__20 1 (p-string-concat "while " $x)))
            (p-post++ $x))
          (p-my-= $x 1)
          (p-while (p-! (p->= $x 4))
            (p-void-ctx
              (funcall
                (lambda ()
                  (progn
                    (p-if (p-! $y__state__19__init)
                      (progn (p-scalar-= $y__state__19 $x) (p-scalar-= $y__state__19__init 1)))
                    $y__state__19))))
            (p-void-ctx (pl-is $y__state__19 1 (p-string-concat "until " $x)))
            (p-post++ $x))
          (p-my-= $x 0)
          (p-my-= $y 0)
          (let ((*package* *package*))
            (block nil
              (tagbody :redo
                (p-void-ctx
                  (funcall
                    (lambda ()
                      (progn
                        (p-if (p-! $z__state__18__init)
                          (progn (p-scalar-= $z__state__18 $x)
                            (p-scalar-= $z__state__18__init 1)))
                        $z__state__18))))
                (p-post++ $z__state__18)
                (p-post++ $y)
                (p-void-ctx (pl-is $z__state__18 $y (p-string-concat "bare block " $y)))
                (p-if (p-< $y 3) (p-redo))
                :next)))
          (p-let ((@simpsons :array (make-array 0 :adjustable t :fill-pointer 0)))
            (p-array-= @simpsons (vector "Homer" "Marge" "Bart" "Lisa" "Maggie"))
            (tagbody :again (p-let (($next :box (make-p-box nil)))
                (p-my-= $next (p-shift @simpsons))
                (p-void-ctx
                  (funcall
                    (lambda ()
                      (progn
                        (p-if (p-! $simpson__state__17__init)
                          (progn (p-scalar-= $simpson__state__17 $next)
                            (p-scalar-= $simpson__state__17__init 1)))
                        $simpson__state__17))))
                (p-void-ctx (pl-is $simpson__state__17 "Homer" "goto 1"))
                (p-if @simpsons (go :again))
                (p-let (($vi :box (make-p-box nil)))
                  (let ((*package* *package*))
                    (block nil
                      (tagbody :redo
                        (catch :pcl-goto-Elvis
                          (p-if (p-! $vi) (throw :pcl-goto-Elvis nil))
                          (p-void-ctx
                            (funcall
                              (lambda ()
                                (progn
                                  (p-if (p-! $calvin__state__16__init)
                                    (progn (p-scalar-= $calvin__state__16 (p-pre++ $vi))
                                      (p-scalar-= $calvin__state__16__init 1)))
                                  $calvin__state__16)))))
                        (tagbody :Elvis
                          (p-void-ctx
                            (funcall
                              (lambda ()
                                (progn
                                  (p-if (p-! $vile__state__15__init)
                                    (progn (p-scalar-= $vile__state__15 (p-pre++ $vi))
                                      (p-scalar-= $vile__state__15__init 1)))
                                  $vile__state__15))))
                          (p-if (p-! (p-defined $calvin__state__16)) (p-redo))
                          (p-void-ctx (pl-is $calvin__state__16 2 "goto 2"))
                          (p-void-ctx (pl-is $vile__state__15 1 "goto 3"))
                          (p-void-ctx (pl-is $vi 2 "goto 4")))
                        :next)))
                  (p-array-= @presidents (vector "Taylor" "Garfield" "Ford" "Arthur" "Monroe"))
                  (p-let (($president_answer :box (make-p-box nil)))
                    (p-my-= $president_answer (p-aref @presidents 0))
                    (p-void-ctx
                      (pl-is (p-scalar (p-scalar-ctx (pl-president)))
                        $president_answer
                        "&goto"))
                    (p-let ((@flowers :array (make-array 0 :adjustable t :fill-pointer 0)))
                      (p-array-= @flowers (vector "Bluebonnet" "Goldenrod" "Hawthorn" "Peony"))
                      (p-foreach ($f @flowers)
                        :my
                        t
                        (p-goto-computed
                          (p-list-ctx
                            (funcall
                              (lambda ()
                                (progn
                                  (p-if (p-! $flower__state__14__init)
                                    (progn (p-scalar-= $flower__state__14 $f)
                                      (p-scalar-= $flower__state__14__init 1)))
                                  $flower__state__14)))))
                        (p-void-ctx (pl-ok 0 "computed goto 0"))
                        (p-next)
                        (tagbody :Bluebonnet
                          (p-void-ctx (pl-ok 1 "computed goto 1"))
                          (p-next)
                          (tagbody :Goldenrod
                            (p-void-ctx (pl-ok 0 "computed goto 2"))
                            (p-next)
                            (tagbody :Hawthorn
                              (p-void-ctx (pl-ok 0 "computed goto 3"))
                              (p-next)
                              (tagbody :Peony
                                (p-void-ctx (pl-ok 0 "computed goto 4"))
                                (p-next)
                                (p-void-ctx (pl-ok 0 "computed goto 5"))
                                (p-next))))))
                      (p-let ((@apollo :array (make-array 0 :adjustable t :fill-pointer 0)))
                        (p-array-= @apollo (vector "Eagle" "Antares" "Odyssey" "Aquarius"))
                        (p-let ((@result1 :array (make-array 0 :adjustable t :fill-pointer 0)))
                          (p-array-= @result1
                            (p-list-ctx
                              (p-map
                                (lambda ($_)
                                  (p-list-ctx
                                    (funcall
                                      (lambda ()
                                        (progn
                                          (p-if (p-! $x__state__13__init)
                                            (progn (p-scalar-= $x__state__13 $_)
                                              (p-scalar-= $x__state__13__init 1)))
                                          $x__state__13)))))
                                @apollo)))
                          (p-let
                            ((@result2 :array (make-array 0 :adjustable t :fill-pointer 0)))
                            (p-array-= @result2
                              (p-list-ctx
                                (p-grep
                                  (lambda ($_)
                                    (funcall
                                      (lambda ()
                                        (progn
                                          (p-if (p-! $x__state__12__init)
                                            (progn
                                              (p-scalar-= $x__state__12
                                                (p-scalar-ctx
                                                  (p-=~ $_
                                                    (p-regex :pat "Eagle"
                                                      :flags ""
                                                      :tier :native))))
                                              (p-scalar-= $x__state__12__init 1)))
                                          $x__state__12))))
                                  @apollo)))
                            (let ((*package* *package*))
                              (block nil
                                (tagbody :redo
                                  ;; local $" = ""
(let ((|$"| (p-box-for-local "")))
                                    (p-void-ctx
                                      (pl-is (p-scalar (p-string-concat (p-join |$"| @result1)))
                                        (p-scalar (p-str-x (p-aref @apollo 0) @apollo))
                                        "map"))
                                    (p-void-ctx
                                      (pl-is (p-scalar (p-string-concat (p-join |$"| @result2)))
                                        (p-scalar (p-string-concat (p-join |$"| @apollo)))
                                        "grep")))
                                  :next)))
                            (p-let (($ref1 :box (make-p-box nil)))
                              (p-my-= $ref1 (p-scalar-ctx (pl-reference)))
                              (p-let (($ref2 :box (make-p-box nil)))
                                (p-my-= $ref2 (p-scalar-ctx (pl-reference)))
                                (p-void-ctx (pl-is $ref1 $ref2 "Reference to state variable"))
                                (p-foreach-range ($x 1 3)
                                  :my
                                  t
                                  (p-pre++ $y__state__10)
                                  $z__state__9
                                  (p-post++ $z__state__9)
                                  (p-void-ctx (pl-is $y__state__10 $x "state pre increment"))
                                  (p-void-ctx (pl-is $z__state__9 $x "state post increment")))
                                (p-let (($tintin :box (make-p-box nil)))
                                  (p-my-= $tintin "Tin-Tin")
                                  (p-let
                                    ((@thunderbirds :array
                                        (make-array 0 :adjustable t :fill-pointer 0)))
                                    (p-array-= @thunderbirds
                                      (vector "Scott" "Virgel" "Alan" "Gordon" "John"))
                                    (p-let
                                      ((@thunderbirds2 :array
                                          (make-array 0 :adjustable t :fill-pointer 0)))
                                      (p-array-= @thunderbirds2
                                        (vector "xcott" "xxott" "xxxtt" "xxxxt" "xxxxx"))
                                      (p-foreach-range ($x 0 4)
                                        :my
                                        t
                                        (p-void-ctx
                                          (funcall
                                            (lambda ()
                                              (progn
                                                (p-if (p-! $c__state__8__init)
                                                  (progn
                                                    (p-scalar-= $c__state__8
                                                      (p-substr-ref $tintin $x 1))
                                                    (p-scalar-= $c__state__8__init 1)))
                                                $c__state__8))))
                                        (p-let (($d :box (make-p-box nil)))
                                          (p-my-= $d
                                            (p-substr-ref
                                              (p-scalar-ctx
                                                (funcall
                                                  (lambda ()
                                                    (progn
                                                      (p-if (p-! $tb__state__7__init)
                                                        (progn
                                                          (p-scalar-= $tb__state__7
                                                            (p-aref @thunderbirds $x))
                                                          (p-scalar-= $tb__state__7__init 1)))
                                                      $tb__state__7))))
                                              $x
                                              1))
                                          (p-setf (p-cast-$ $c__state__8) "x")
                                          (p-setf (p-cast-$ $d) "x")
                                          (p-void-ctx (pl-is $tintin "xin-Tin" "substr"))
                                          (p-void-ctx
                                            (pl-is $tb__state__7
                                              (p-scalar (p-aref-argbox @thunderbirds2 $x))
                                              "substr"))))
                                      (p-let
                                        ((@spam :array
                                            (make-array 0 :adjustable t :fill-pointer 0)))
                                        (p-array-= @spam (vector "spam" "ham" "bacon" "beans"))
                                        (p-foreach ($spam @spam)
                                          :my
                                          t
                                          (progn ;; RULED REFUSAL: given/when (feature 'switch') is not supported -- removed in perl 5.42
 (pcl:p-die (p-esc "PCL: given/when (feature 'switch') is not supported -- removed in perl 5.42, at state.t line 350\\n"))))
                                        (let ((*package* *package*))
                                          (block nil
                                            (tagbody :redo
                                              (p-void-ctx
                                                (funcall
                                                  (lambda ()
                                                    (progn
                                                      (p-if (p-! $x__state__5__init)
                                                        (progn (p-scalar-= $x__state__5 "one")
                                                          (p-scalar-= $x__state__5__init 1)))
                                                      $x__state__5))))
                                              (p-void-ctx
                                                (funcall
                                                  (lambda ()
                                                    (progn
                                                      (p-if (p-! $x__state__4__init)
                                                        (progn (p-scalar-= $x__state__4 "two")
                                                          (p-scalar-= $x__state__4__init 1)))
                                                      $x__state__4))))
                                              (p-void-ctx (pl-is $x__state__4 "two" "masked"))
                                              :next)))
                                        (let ((*package* *package*))
                                          (p-dyn-once
                                            (block nil
                                              (tagbody :redo
                                                (p-let
                                                  ((@f :array
                                                      (make-array 0 :adjustable t :fill-pointer 0)))
                                                  ;; push @f, do { my $x__state__3; sub {  $x__state__3; ++$x__state__3 } } for 1..2
(p-foreach ($_ (p-.. 1 2)) (p-push @f (p-list-ctx (funcall (lambda () (progn (p-let (($x__state__3 :box (make-p-box nil) :perl "$x" :why :state-cell :captured t)) (lambda (&rest %_args) (let ((@_ (p-flatten-args %_args)) (*pcl-current-package* "main") (*pcl-caller-wantarray* *wantarray*)) (p-sub-frame (block nil (p-void-ctx $x__state__3 (p-caller-ctx (p-pre++ $x__state__3))))))))))))))
                                                  ;; $f[0]->() for 1..10
(p-foreach ($_ (p-.. 1 10)) :dyn t (p-void-ctx (p-funcall-ref (p-aref @f 0))))
                                                  (p-void-ctx
                                                    (pl-is
                                                      (p-scalar
                                                        (p-scalar-ctx
                                                          (p-funcall-ref (p-aref @f 0))))
                                                      11))
                                                  (p-void-ctx
                                                    (pl-is
                                                      (p-scalar
                                                        (p-scalar-ctx
                                                          (p-funcall-ref (p-aref @f 1))))
                                                      1)))
                                                :next))))
                                        (let ((*package* *package*))
                                          (p-dyn-once
                                            (block nil
                                              (tagbody :redo
                                                (p-let (($x :box (make-p-box nil) :captured t))
                                                  (p-let
                                                    ((@f :array
                                                        (make-array 0 :adjustable t :fill-pointer 0)))
                                                    ;; push @f, do { my $s__state__2; my $s__state__2__init; sub { $x=0; do { unless ($s__state__2__init) {  $s__state__2 = $_[0] ; $s__state__2__init = 1 } $s__state__2 }; $s__state__2 } } for 1..2
(p-foreach ($_ (p-.. 1 2)) (p-push @f (p-list-ctx (funcall (lambda () (progn (p-let (($s__state__2 :box (make-p-box nil) :perl "$s" :why :state-cell :captured t)) (p-let (($s__state__2__init :box (make-p-box nil) :captured t)) (lambda (&rest %_args) (let ((@_ (p-flatten-args %_args)) (*pcl-current-package* "main") (*pcl-caller-wantarray* *wantarray*)) (p-sub-frame (block nil (p-void-ctx (p-my-= $x 0) (funcall (lambda () (progn (p-if (p-! $s__state__2__init) (progn (p-my-= $s__state__2 (p-aref @_ 0)) (p-my-= $s__state__2__init 1))) $s__state__2))) (p-caller-ctx $s__state__2))))))))))))))
                                                    (p-void-ctx
                                                      (pl-is
                                                        (p-scalar
                                                          (p-scalar-ctx
                                                            (p-funcall-ref (p-aref @f 0) 1)))
                                                        1))
                                                    (p-void-ctx
                                                      (pl-is
                                                        (p-scalar
                                                          (p-scalar-ctx
                                                            (p-funcall-ref (p-aref @f 0) 2)))
                                                        1))
                                                    (p-void-ctx
                                                      (pl-is
                                                        (p-scalar
                                                          (p-scalar-ctx
                                                            (p-funcall-ref (p-aref @f 1) 3)))
                                                        3))
                                                    (p-void-ctx
                                                      (pl-is
                                                        (p-scalar
                                                          (p-scalar-ctx
                                                            (p-funcall-ref (p-aref @f 1) 4)))
                                                        3))))
                                                :next))))
                                        (let ((*package* *package*))
                                          (block nil
                                            (tagbody :redo
                                              (p-let
                                                ((@forbidden_items :array
                                                    (make-array 0 :adjustable t :fill-pointer 0)))
                                                (p-array-= @forbidden_items
                                                  (p-list-ctx
                                                    (funcall (lambda () (progn (progn))))))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a) = 1 â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state @a) = 1 â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state @a :shared) = 1 â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state %a) = () â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state %a :shared) = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a) = 1 â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state ($a)) = 1 â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a) = 1 â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a)) = 1 â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a) :shared = 1 â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a) :shared) = 1 â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (%a) = () â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (%a)) = () â invalid state list assignment, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (%a) :shared = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (%a) :shared) = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (undef, $a) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (undef, $a)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (undef, @a) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (undef, @a)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a, undef) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state ($a, undef)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a, $b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state ($a, $b)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a, $b) :shared = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state ($a, $b) :shared) = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a, @b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state ($a, @b)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a, @b) :shared = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state ($a, @b) :shared) = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a, undef) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a, undef)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a, $b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a, $b)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a, $b) :shared = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a, $b) :shared) = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a, @b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a, @b)) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state (@a, @b) :shared = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state (@a, @b) :shared) = () â needs attributes.pm, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a, state $b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a, $b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a, my $b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a, state @b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a, local @b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: (state $a, undef, state $b) = () â invalid state list, PCL does not reject"))
                                                (p-void-ctx
                                                  (pl-ok 1
                                                    "SKIP: state ($a, undef, $b) = () â invalid state list, PCL does not reject")))
                                              :next)))
                                        (let ((*package* *package*))
                                          (p-dyn-once
                                            (block nil
                                              (tagbody :redo
                                                (p-let
                                                  ((@warnings :array
                                                      (make-array 0 :adjustable t :fill-pointer 0)))
                                                  ;; local $SIG{"__WARN__"} = sub { push @warnings, $_[0] }
(p-local-hash-elem-init %SIG "__WARN__" (lambda (&rest %_args) (let ((@_ (p-flatten-args %_args)) (*pcl-current-package* "main") (*pcl-caller-wantarray* *wantarray*)) (p-sub-frame (block nil (p-push @warnings (p-aref @_ 0))))))
                                                    (p-eval
                                                      (p-esc
                                                        "\\n\\tuse warnings;\\n\\n\\tsub f_49522 {\\n\\t    state $s = 88;\\n\\t    sub g_49522 { $s }\\n\\t    sub { $s };\\n\\t}\\n\\n\\tsub h_49522 {\\n\\t    state $t = 99;\\n\\t    sub i_49522 {\\n\\t\\tsub { $t };\\n\\t    }\\n\\t}\\n    ")
                                                      (list
                                                        (cons "$f1" $f1)
                                                        (cons "$f2" $f2)
                                                        (cons "$next" $next)
                                                        (cons "$president_answer"
                                                          $president_answer)
                                                        (cons "$ref1" $ref1)
                                                        (cons "$ref2" $ref2)
                                                        (cons "$t" $t)
                                                        (cons "$tintin" $tintin)
                                                        (cons "$vi" $vi)
                                                        (cons "$x" $x)
                                                        (cons "$xhval" $xhval)
                                                        (cons "$xsize" $xsize)
                                                        (cons "$y" $y)
                                                        (cons "$z" $z)
                                                        (cons "@apollo" @apollo)
                                                        (cons "@flowers" @flowers)
                                                        (cons "@result1" @result1)
                                                        (cons "@result2" @result2)
                                                        (cons "@simpsons" @simpsons)
                                                        (cons "@spam" @spam)
                                                        (cons "@thunderbirds" @thunderbirds)
                                                        (cons "@thunderbirds2" @thunderbirds2)
                                                        (cons "@warnings" @warnings)))
                                                    (p-void-ctx (pl-is $@ "" "eval f_49522"))
                                                    (p-void-ctx
                                                      (pl-ok (p-scalar (p-! @warnings))
                                                        (p-string-concat
                                                          "suppress warnings part 1 ["
                                                          (p-join |$"| @warnings)
                                                          "]")))
                                                    (p-array-= @warnings (vector))
                                                    (p-let (($f :box (make-p-box nil)))
                                                      (p-my-= $f (p-scalar-ctx (pl-f_49522)))
                                                      (p-void-ctx
                                                        (pl-is
                                                          (p-scalar
                                                            (p-scalar-ctx (p-funcall-ref $f)))
                                                          88
                                                          "state var closure 1"))
                                                      (p-void-ctx
                                                        (pl-is
                                                          (p-scalar (p-scalar-ctx (pl-g_49522)))
                                                          88
                                                          "state var closure 2"))
                                                      (p-void-ctx
                                                        (pl-ok (p-scalar (p-! @warnings))
                                                          (p-string-concat
                                                            "suppress warnings part 2 ["
                                                            (p-join |$"| @warnings)
                                                            "]")))
                                                      (p-array-= @warnings (vector))
                                                      (p-my-= $f (p-scalar-ctx (pl-i_49522)))
                                                      (p-void-ctx (pl-h_49522))
                                                      (p-void-ctx
                                                        (pl-is
                                                          (p-scalar
                                                            (p-scalar-ctx (p-funcall-ref $f)))
                                                          99
                                                          "state var closure 3"))
                                                      (p-void-ctx
                                                        (pl-ok (p-scalar (p-! @warnings))
                                                          (p-string-concat
                                                            "suppress warnings part 3 ["
                                                            (p-join |$"| @warnings)
                                                            "]"))))))
                                                :next))))
                                        (let ((*package* *package*))
                                          (block nil
                                            (tagbody :redo
                                              (p-void-ctx
                                                (funcall
                                                  (lambda ()
                                                    (progn
                                                      (p-if (p-! $f__state__1__init)
                                                        (progn (p-scalar-= $f__state__1 1)
                                                          (p-scalar-= $f__state__1__init 1)))
                                                      $f__state__1))))
                                              (p-if 0 (p-void-ctx (pl-foo $f__state__1)))
                                              (p-void-ctx
                                                (pl-ok (p-scalar (p-defined $f__state__1))
                                                  "state init not skipped"))
                                              :next)))
                                        (let ((*package* *package*))
                                          (block nil
                                            (tagbody :redo
                                              (p-void-ctx (pl-thing (p-list-ctx (p-undef))))
                                              (p-void-ctx (pl-thing 5))
                                              (p-void-ctx (pl-thing2 (p-list-ctx (p-undef))))
                                              (p-void-ctx (pl-thing2 6))
                                              :next)))
                                        (p-void-ctx
                                          (pl-ok (p-scalar (p-scalar-ctx (pl-rt_123029)))
                                            "state variables don't surprisingly disappear when accessed"))
                                        (p-foreach ($_ (vector 1 2))
                                          (p-void-ctx
                                            (funcall
                                              (lambda ()
                                                (progn
                                                  (p-if (p-! $s__state__0__init)
                                                    (progn
                                                      (p-scalar-= $s__state__0
                                                        (p-string-concat "-" $_ "-"))
                                                      (p-scalar-= $s__state__0__init 1)))
                                                  $s__state__0))))
                                          (p-void-ctx
                                            (pl-is $s__state__0
                                              "-1-"
                                              (p-string-concat "state with multiconcat pass "
                                                $_))))
                                        (let ((*package* *package*))
                                          (p-dyn-once
                                            (block nil
                                              (tagbody :redo
                                                (p-let
                                                  ((@warnings :array
                                                      (make-array 0 :adjustable t :fill-pointer 0)))
                                                  ;; local $SIG{"__WARN__"} = sub { push @warnings, @_ }
(p-local-hash-elem-init %SIG "__WARN__" (lambda (&rest %_args) (let ((@_ (p-flatten-args %_args)) (*pcl-current-package* "main") (*pcl-caller-wantarray* *wantarray*)) (p-sub-frame (block nil (p-push @warnings @_)))))
                                                    (p-let (($e :box (make-p-box nil)))
                                                      (p-my-= $e
                                                        (p-eval
                                                          "my $s = sub { state sub FOO () { 42 } }; 1;"
                                                          (list
                                                            (cons "$e" $e)
                                                            (cons "$f1" $f1)
                                                            (cons "$f2" $f2)
                                                            (cons "$next" $next)
                                                            (cons "$president_answer"
                                                              $president_answer)
                                                            (cons "$ref1" $ref1)
                                                            (cons "$ref2" $ref2)
                                                            (cons "$t" $t)
                                                            (cons "$tintin" $tintin)
                                                            (cons "$vi" $vi)
                                                            (cons "$x" $x)
                                                            (cons "$xhval" $xhval)
                                                            (cons "$xsize" $xsize)
                                                            (cons "$y" $y)
                                                            (cons "$z" $z)
                                                            (cons "@apollo" @apollo)
                                                            (cons "@flowers" @flowers)
                                                            (cons "@result1" @result1)
                                                            (cons "@result2" @result2)
                                                            (cons "@simpsons" @simpsons)
                                                            (cons "@spam" @spam)
                                                            (cons "@thunderbirds"
                                                              @thunderbirds)
                                                            (cons "@thunderbirds2"
                                                              @thunderbirds2)
                                                            (cons "@warnings" @warnings))))
                                                      (p-void-ctx
                                                        (pl-is $e 1 "const state sub ran ok"))
                                                      (p-or
                                                        (p-scalar-ctx
                                                          (pl-ok (p-scalar (p-! @warnings))
                                                            "no 'Attempt to free unreferenced scalar'"))
                                                        (p-void-ctx
                                                          (pl-diag
                                                            (p-string-concat
                                                              (p-esc "got these warnings:\\n")
                                                              (p-join |$"| @warnings))))))))
                                                :next)))))))))))))))))))))))))

