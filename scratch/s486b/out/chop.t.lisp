;;; pcl: pipeline=v2 gen=v2-1460
(in-package :pcl)
(setf pcl::*pcl-pl2cl-path* #P"/home/bernt/pcl/.claude/worktrees/agent-a6c7caf694eb1f8c2/pl2cl")
(pcl::box-set pcl::$0 "chop.t")
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

(pcl:p-defpackage :utf8)

(p-declare-sub pl-foo)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $a__excl__0 (make-p-box nil) :perl "$a" :why :exception-global)
(p-defcell $asc (make-p-box nil))
(p-defcell $b__excl__1 (make-p-box nil) :perl "$b" :why :exception-global)
(p-defcell $c (make-p-box nil))
(p-defcell $chomped (make-p-box nil))
(p-defcell $end (make-p-box nil))
(p-defcell $end_utf8 (make-p-box nil))
(p-defcell $err (make-p-box nil))
(p-defcell $expected (make-p-box nil))
(p-defcell $foo (make-p-box nil))
(p-defcell $got (make-p-box nil))
(p-defcell $input (make-p-box nil))
(p-defcell $key (make-p-box nil))
(p-defcell $message (make-p-box nil))
(p-defcell $result (make-p-box nil))
(p-defcell $start (make-p-box nil))
(p-defcell $string (make-p-box nil))
(p-defcell $tests_count (make-p-box nil))
(p-defcell $uid (make-p-box nil))
(p-defcell $utf (make-p-box nil))
(p-defcell %Config (make-hash-table :test 'equal))
(p-defcell %chomp (make-hash-table :test 'equal))
(p-defcell %chop (make-hash-table :test 'equal))
(p-defcell %stuff (make-hash-table :test 'equal))
(p-defcell @a (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @bar (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @chars (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @foo (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @line (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @stuff (make-array 0 :adjustable t :fill-pointer 0))


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

  ;; require './charset_tools.pl'

  (p-eval-always

    (p-require-file "./charset_tools.pl"))

)

(p-sub pl-foo
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-chop $_))))

;; use Config

(p-eval-always

  (p-use "Config"))

(p-eval-always (p-note-inc "warnings"))

(p-run-compile-phase-blocks)

(p-let (($tests_count :box (make-p-box nil)))
  (p-my-= $tests_count 148)
  (p-void-ctx (pl-plan "tests" $tests_count))
  (p-scalar-= $_ "abc")
  (p-scalar-= $c (p-scalar-ctx (pl-foo)))
  (p-void-ctx (pl-is (p-scalar (p-. $c $_)) "cab" "optimized"))
  (p-scalar-= $_ "abc")
  (p-scalar-= $c (p-chop $_))
  (p-void-ctx (pl-is (p-scalar (p-. $c $_)) "cab" "unoptimized"))
  (p-array-= @foo (vector (p-esc "hi \\n") (p-esc "there\\n") (p-esc "!\\n")))
  (p-array-= @bar @foo)
  (p-chop @bar)
  (p-void-ctx
    (pl-is (p-scalar (p-list-ctx (p-join "" @bar))) "hi there!" "chop list of strings"))
  (p-scalar-= $foo (p-esc "\\n"))
  (p-chop $foo @foo)
  (p-void-ctx
    (pl-is (p-scalar (p-list-ctx (p-join "" $foo @foo)))
      "hi there!"
      "chop on list reduces one-character element to an empty string"))
  (p-scalar-= $_ (p-esc "foo\\n\\n"))
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      1
      "check return value when chomp string ending with two newlines; $/ is set to default of one newline"))
  (p-void-ctx
    (pl-is $_
      (p-esc "foo\\n")
      "chomp string ending with two newlines while $/ is set to one newline"))
  (p-scalar-= $_ (p-esc "foo\\n"))
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      1
      "check return value chomp string ending with one newline while $/ is set to a newline"))
  (p-void-ctx
    (pl-is $_
      "foo"
      "test typical use of chomp; chomp a string ending in a single newline while $/ is set to default of one newline"))
  (p-scalar-= $_ "foo")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      0
      "check return value when chomp a string that does not end with current value of $/, 0 should be returned"))
  (p-void-ctx (pl-is $_ "foo" "chomp a string that does not end with the current value of $/"))
  (p-scalar-= $_ "foo")
  (p-setf |$/| "oo")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      "2"
      "check return value when chomp string with $/ consisting of more than one character, and with the ending of the string matching $/"))
  (p-void-ctx
    (pl-is $_
      "f"
      "chomp a string when $/ consists of two characters that are at the end of the string, check that chomped string contains remnant of original string"))
  (p-scalar-= $_ "bar")
  (p-setf |$/| "oo")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      "0"
      "check return value when call chomp with $/ consisting of more than one character, and with the ending of the string NOT matching $/"))
  (p-void-ctx
    (pl-is $_
      "bar"
      "chomp a string when $/ consists of two characters that are NOT at the end of the string"))
  (p-scalar-= $_ (p-esc "f\\n\\n\\n\\n\\n"))
  (p-setf |$/| "")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      5
      "check return value when chomp in paragraph mode on string ending with 5 newlines"))
  (p-void-ctx (pl-is $_ "f" "chomp in paragraph mode on string ending with 5 newlines"))
  (p-scalar-= $_ (p-esc "f\\n\\n"))
  (p-setf |$/| "")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      2
      "check return value when chomp in paragraph mode on string ending with 2 newlines"))
  (p-void-ctx (pl-is $_ "f" "chomp in paragraph mode on string ending with 2 newlines"))
  (p-scalar-= $_ (p-esc "f\\n"))
  (p-setf |$/| "")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      1
      "check return value when chomp in paragraph mode on string ending with 1 newline"))
  (p-void-ctx (pl-is $_ "f" "chomp in paragraph mode on string ending with 1 newlines"))
  (p-scalar-= $_ "f")
  (p-setf |$/| "")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      0
      "check return value when chomp in paragraph mode on string ending with no newlines"))
  (p-void-ctx (pl-is $_ "f" "chomp in paragraph mode on string lacking trailing newlines"))
  (p-scalar-= $_ "xx")
  (p-setf |$/| "xx")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      2
      "check return value when chomp string that consists solely of current value of $/"))
  (p-void-ctx
    (pl-is $_
      ""
      "chomp on string that consists solely of current value of $/; check that empty string remains"))
  (p-scalar-= $_ "axx")
  (p-setf |$/| "xx")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx
    (pl-is $got
      2
      "check return value when chomp string that ends with current value of $/. $/ contains two characters"))
  (p-void-ctx
    (pl-is $_
      "a"
      "check that when chomp string that ends with currnt value of $/, the part of original string that wasn't in $/ remains"))
  (p-scalar-= $_ "axx")
  (p-setf |$/| "yy")
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx (pl-is $got 0 "check return value when chomp string that does not end with $/"))
  (p-void-ctx
    (pl-is $_
      "axx"
      "chomp a string that does not end with $/, the entire string should remain intact"))
  (p-scalar-= $_ (p-esc "ab\\n"))
  (p-setf |$/| (p-backslash 3))
  (p-scalar-= $got (p-chomp $_))
  (p-void-ctx (pl-is $got 0 "check return value when call chomp with $_ = \"ab\\n\", $/ = \\3"))
  (p-void-ctx (pl-is $_ (p-esc "ab\\n") "chomp with $_ = \"ab\\n\", $/ = \\3"))
  (p-scalar-= $_ "abcሴ")
  (p-chop $_)
  (p-void-ctx (pl-is $_ "abc" "Go Unicode"))
  (p-scalar-= $_ "abcሴd")
  (p-chop $_)
  (p-void-ctx (pl-is $_ "abcሴ"))
  (p-scalar-= $_ "ሴ⍅")
  (p-chop $_)
  (p-void-ctx (pl-is $_ "ሴ"))
  (p-let ((@stuff :array (make-array 0 :adjustable t :fill-pointer 0)))
    (p-array-= @stuff (vector "this" "that"))
    (p-void-ctx (pl-is (p-scalar (p-chop (p-aslice @stuff 0 1))) "t"))
    (p-array-= @stuff (vector "ab" "cd" "ef"))
    (p-void-ctx (pl-is (p-scalar (p-chop (p-array-= @stuff @stuff))) "f"))
    (p-array-= @stuff (vector "ab" "cd" "ef"))
    (p-void-ctx (pl-is (p-scalar (p-chop (p-aslice @stuff 0 2))) "f"))
    (p-let ((%stuff :hash (make-hash-table :test 'equal)))
      (p-scalar-ctx (p-hash-= %stuff (vector (p-.. 1 4))))
      (p-void-ctx (pl-is (p-scalar (p-chop (p-hslice %stuff 1 3))) "4"))
      (p-scalar-= $_ (make-p-box (make-array 0 :adjustable t :fill-pointer 0)))
      (p-setf |$/| (p-esc "\\n"))
      (p-scalar-= $got (p-chomp $_))
      (p-or (p-scalar-ctx (pl-ok (p-scalar (p-== $got 0))))
        (p-print (p-string-concat "# got " $got (p-esc "\\n"))))
      (p-void-ctx (pl-is (p-scalar (p-ref $_)) "ARRAY" "chomp ref (modify)"))
      (p-setf |$/| ")")
      (p-scalar-= $got (p-chomp $_))
      (p-or (p-scalar-ctx (pl-ok (p-scalar (p-== $got 1))))
        (p-print (p-string-concat "# got " $got (p-esc "\\n"))))
      (p-void-ctx (pl-ok (p-scalar (p-! (p-ref $_))) "chomp ref (no modify)"))
      (p-setf |$/| (p-esc "\\n"))
      (p-hash-= %chomp (vector "One" "One" (p-esc "Two\\n") "Two" "" ""))
      (p-hash-= %chop (vector "One" "On" (p-esc "Two\\n") "Two" "" ""))
      (p-foreach ($_ (p-list-ctx (p-keys %chomp)))
        (p-let (($key :box (make-p-box nil)))
          (p-my-= $key $_)
          (p-void-ctx (p-eval-block (p-chomp $_)))
          (p-if $@
            (progn
              (p-let (($err :box (make-p-box nil)))
                (p-my-= $err $@)
                (p-=~ $err (p-subst :pat "\\n$" :rep "" :flags "s" :tier :native))
                (p-void-ctx (pl-fail (p-string-concat "$@ = \"" $err "\"")))))
            (progn
              (p-void-ctx (pl-is $_ (p-scalar (p-gethash-argbox %chomp $key)) "chomp hash key"))))))
      (p-foreach ($_ (p-list-ctx (p-keys %chop)))
        (p-let (($key :box (make-p-box nil)))
          (p-my-= $key $_)
          (p-void-ctx (p-eval-block (p-chop $_)))
          (p-if $@
            (progn
              (p-let (($err :box (make-p-box nil)))
                (p-my-= $err $@)
                (p-=~ $err (p-subst :pat "\\n$" :rep "" :flags "s" :tier :native))
                (p-void-ctx (pl-fail (p-string-concat "$@ = \"" $err "\"")))))
            (progn
              (p-void-ctx (pl-is $_ (p-scalar (p-gethash-argbox %chop $key)) "chop hash key"))))))
      (p-eval "chop($x) = 1;"
        (list (cons "$tests_count" $tests_count) (cons "%stuff" %stuff) (cons "@stuff" @stuff)))
      (p-void-ctx
        (pl-ok
          (p-scalar
            (p-scalar-ctx
              (p-=~ $@
                (p-regex :pat "Can\\'t modify.*chop.*in.*assignment" :flags "" :tier :native))))))
      (p-eval "chomp($x) = 1;"
        (list (cons "$tests_count" $tests_count) (cons "%stuff" %stuff) (cons "@stuff" @stuff)))
      (p-void-ctx
        (pl-ok
          (p-scalar
            (p-scalar-ctx
              (p-=~ $@
                (p-regex :pat "Can\\'t modify.*chom?p.*in.*assignment" :flags "" :tier :native))))))
      (p-eval "chop($x, $y) = (1, 2);"
        (list (cons "$tests_count" $tests_count) (cons "%stuff" %stuff) (cons "@stuff" @stuff)))
      (p-void-ctx
        (pl-ok
          (p-scalar
            (p-scalar-ctx
              (p-=~ $@
                (p-regex :pat "Can\\'t modify.*chop.*in.*assignment" :flags "" :tier :native))))))
      (p-eval "chomp($x, $y) = (1, 2);"
        (list (cons "$tests_count" $tests_count) (cons "%stuff" %stuff) (cons "@stuff" @stuff)))
      (p-void-ctx
        (pl-ok
          (p-scalar
            (p-scalar-ctx
              (p-=~ $@
                (p-regex :pat "Can\\'t modify.*chom?p.*in.*assignment" :flags "" :tier :native))))))
      (p-let ((@chars :array (make-array 0 :adjustable t :fill-pointer 0)))
        (p-array-= @chars
          (vector "N"
            (p-list-ctx (pl-uni_to_native "Ó"))
            (p-list-ctx (p-substr (p-. (p-scalar-ctx (pl-uni_to_native "Ô")) "Ā") 0 1))
            (p-list-ctx (p-chr 1296))))
        (p-foreach ($start @chars)
          :my
          t
          (p-foreach ($end @chars)
            :my
            t
            ;; local $/ = $end
(let (($/ (p-box-for-local $end)))
              (p-let (($message :box (make-p-box nil)))
                (p-my-= $message
                  (p-. (p-. (p-. "start=" (p-ord $start)) " end=") (p-ord $end)))
                (p-let (($string :box (make-p-box nil)))
                  (p-my-= $string (p-. $start $end))
                  (p-void-ctx
                    (pl-is (p-scalar (p-chomp $string))
                      1
                      (p-string-concat $message " [returns 1]")))
                  (p-void-ctx (pl-is $string $start $message))
                  (p-let (($end_utf8 :box (make-p-box nil)))
                    (p-my-= $end_utf8 $end)
                    (p-void-ctx (utf8::pl-encode $end_utf8))
                    (p-if (p-str-eq $end_utf8 $end) (p-next))
                    (p-my-= $string (p-. $start $end_utf8))
                    (p-let (($chomped :box (make-p-box nil)))
                      (p-my-= $chomped $string)
                      (p-void-ctx
                        (pl-is (p-scalar (p-chomp $chomped))
                          0
                          (p-string-concat $message " (end as bytes) [returns 0]")))
                      (p-void-ctx
                        (pl-is $chomped $string (p-string-concat $message " (end as bytes)")))
                      (p-setf |$/| $end_utf8)
                      (p-my-= $string (p-. $start $end))
                      (p-my-= $chomped $string)
                      (p-void-ctx
                        (pl-is (p-scalar (p-chomp $chomped))
                          0
                          (p-string-concat $message " ($/ as bytes) [returns 0]")))
                      (p-void-ctx
                        (pl-is $chomped $string (p-string-concat $message " ($/ as bytes)"))))))))))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-setf |$/| "Ā")
              (p-scalar-= $a (p-string-concat "A" |$/|))
              (p-scalar-= $b (p-chomp $a))
              (p-void-ctx (pl-is $b 1))
              (p-setf |$/| "Āā")
              (p-scalar-= $a (p-string-concat "A" |$/|))
              (p-scalar-= $b (p-chomp $a))
              (p-void-ctx (pl-is $b 2))
              :next)))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-let (($asc :box (make-p-box nil)))
                (p-my-= $asc (p-esc "perl\\u0000"))
                (p-let (($utf :box (make-p-box nil)))
                  (p-my-= $utf (p-. "perl" (p-pack "U" 0)))
                  (p-void-ctx
                    (pl-is (p-scalar (p-chop $asc)) (p-esc "\\u0000") "chopping ascii NUL"))
                  (p-void-ctx
                    (pl-is (p-scalar (p-chop $utf)) (p-esc "\\u0000") "chopping utf8 NUL"))
                  (p-void-ctx (pl-is $asc "perl" "chopped ascii NUL"))
                  (p-void-ctx (pl-is $utf "perl" "chopped utf8 NUL"))))
              :next)))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-map (lambda ($_) (p-chop (vector))) (p-list-x (vector "") 68))
              (p-void-ctx (pl-ok 1 "extend sp in pp_chop"))
              (p-map (lambda ($_) (p-chomp (vector))) (p-list-x (vector "") 68))
              (p-void-ctx (pl-ok 1 "extend sp in pp_chomp"))
              :next)))
        (let ((*package* *package*))
          (block SKIP
            (catch (pcl::%pcl-loop-tag "LAST" 'SKIP)
              (block nil
                (catch (pcl::%pcl-loop-tag "NEXT" 'SKIP)
                  (tagbody :redo
                    (catch (pcl::%pcl-loop-tag "REDO" 'SKIP)
                      (progn
                        (p-or (p-> (p-gethash %Config "ivsize") 4)
                          (p-void-ctx
                            (pl-skip "this build can't handle very large characters" 4)))
                        (p-let (($utf :box (make-p-box nil)))
                          (p-my-= $utf (p-. (p-chr #x80000001) (p-chr #x80000000)))
                          (p-let (($result :box (make-p-box nil)))
                            (p-my-= $result (p-chop $utf))
                            (p-void-ctx
                              (pl-is $utf
                                (p-scalar (p-chr #x80000001))
                                "chopping high 'unicode'- remnant"))
                            (p-void-ctx
                              (pl-is $result
                                (p-scalar (p-chr #x80000000))
                                "chopping high 'unicode' - result"))
                            (p-my-= $utf
                              (p-. (p-chr #x7fffffffffffffff) (p-chr #x7ffffffffffffffe)))
                            (p-my-= $result (p-chop $utf))
                            (p-void-ctx
                              (pl-is $utf
                                (p-scalar (p-chr #x7fffffffffffffff))
                                "chop even higher 'unicode'- remnant"))
                            (p-void-ctx
                              (pl-is $result
                                (p-scalar (p-chr #x7ffffffffffffffe))
                                "chop even higher 'unicode' - result"))))
                        (go :next)))
                    (go :redo)
                    :next))))))
        (p-setf |$/| (p-esc "\\n"))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-let (($expected :box (make-p-box nil)))
                (p-my-= $expected 99999)
                (p-let (($input :box (make-p-box nil)))
                  (p-my-= $input
                    (p-string-concat (p-esc "UserID\\talpha ") $expected (p-esc "\\n")))
                  (p-let (($uid :box (make-p-box nil)))
                    (p-my-= $uid "")
                    (p-let ((@line :array (make-array 0 :adjustable t :fill-pointer 0)))
                      (p-chomp
                        (p-array-= @line
                          (p-split (p-regex :pat " |\\t" :flags "" :tier :native) $input)))
                      (p-my-= $uid (p-aref @line -1))
                      (p-void-ctx
                        (pl-is $uid $expected "RT #123057: chomp works as expected on split"))))))
              :next)))
        (let ((*package* *package*))
          (block nil
            (tagbody :redo
              (p-let (($a__excl__0 :box (make-p-box nil) :perl "$a" :why :exception-global))
                (p-my-= $a__excl__0 (p-setf |$/| 7))
                (p-my-= $a__excl__0 (p-chomp $a__excl__0))
                (p-void-ctx (pl-is $a__excl__0 1 "lexical $a = chomp $a when $a eq $/ eq 7"))
                (p-my-= $a__excl__0 (p-setf |$/| 0))
                (p-my-= $a__excl__0 (p-chomp $a__excl__0))
                (p-void-ctx (pl-is $a__excl__0 1 "lexical $a = chomp $a when $a eq $/ eq 0"))
                (p-let ((@a :array (make-array 0 :adjustable t :fill-pointer 0)))
                  (p-array-= @a "7")
                  (p-foreach ($b__excl__1 (vector (p-aref-box @a 0)))
                    :my
                    t
                    (p-setf |$/| 7)
                    (p-my-= $b__excl__1 (p-chomp @a))
                    (p-void-ctx
                      (pl-is $b__excl__1
                        1
                        "lexical $b = chomp @a when $b eq $/ eq 7 and \\$a[0] == \\$b"))
                    (p-my-= $b__excl__1 (p-setf |$/| 0))
                    (p-my-= $b__excl__1 (p-chomp @a))
                    (p-void-ctx
                      (pl-is $b__excl__1
                        1
                        "lexical $b = chomp @a when $b eq $/ eq 0 and \\$a[0] == \\$b")))))
              :next)))))))

