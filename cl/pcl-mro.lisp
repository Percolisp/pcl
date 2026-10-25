;;; pcl: pipeline=v2 gen=v2-8785
;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

(in-package :pcl)
(p-defpackage :main)
(in-package :main)
(pcl::p-eval-always (pcl::p-detect-table "lib/mro.pm"
 '(("mro" "%" 73 "unknown")
   ("mro" "_c3_linearize" 37 "list-call")
   ("mro" "_c3_linearize" 63 "list-call")
   ("mro" "_c3_linearize" 83 "list-call")
   ("mro" "defined" 105 "builtin")
   ("mro" "defined" 63 "builtin")
   ("mro" "die" 105 "builtin")
   ("mro" "die" 63 "builtin")
   ("mro" "die" 75 "builtin")
   ("mro" "grep" 63 "builtin")
   ("mro" "grep" 88 "builtin")
   ("mro" "if" 63 "builtin")
   ("mro" "last" 100 "builtin")
   ("mro" "last" 102 "builtin")
   ("mro" "last" 89 "builtin")
   ("mro" "last" 98 "builtin")
   ("mro" "map" 63 "builtin")
   ("mro" "map" 83 "builtin")
   ("mro" "push" 108 "builtin")
   ("mro" "push" 63 "builtin")
   ("mro" "push" 84 "builtin")
   ("mro" "scalar" 63 "builtin")
   ("mro" "scalar" 88 "builtin")
   ("mro" "shift" 112 "builtin")
   ("mro" "shift" 63 "builtin")
   ("mro" "unless" 63 "builtin"))
 '(("mro" "import" nil 29)
   ("mro" "unimport" nil 30)
   ("mro" "get_linear_isa" nil 35)
   ("mro" "get_mro" nil 43)
   ("mro" "set_mro" nil 44)
   ("mro" "get_isarev" nil 49)
   ("mro" "is_universal" nil 51)
   ("mro" "invalidate_all_method_caches" nil 62)
   ("mro" "method_changed_in" nil 63)
   ("mro" "_c3_linearize" nil 67))))

(pcl:p-defpackage :mro)

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



;;; package mro
(p-defpackage :mro)
(in-package :mro)
(p-defclass plc-mro () ())
(p-register-pkg-name :mro "mro")

(p-declare-sub pl-import :body)
(p-declare-sub pl-unimport :body)
(p-declare-sub pl-get_linear_isa :body)
(p-declare-sub pl-get_mro :body)
(p-declare-sub pl-set_mro :body)
(p-declare-sub pl-get_isarev :body)
(p-declare-sub pl-is_universal :body)
(p-declare-sub pl-invalidate_all_method_caches :body)
(p-declare-sub pl-method_changed_in :body)
(p-declare-sub pl-_c3_linearize :body)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $cand (make-p-box nil))
(p-defcell $class (make-p-box nil))
(p-defcell $head (make-p-box nil))
(p-defcell $i (make-p-box nil))
(p-defcell $in_tail (make-p-box nil))
(p-defcell $s (make-p-box nil))
(p-defcell $seen (make-p-box nil))
(p-defcell $seq (make-p-box nil))
(p-defcell $type (make-p-box nil))
(p-defcell $u (make-p-box nil))
(p-defcell @parents (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @result (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell @seqs (make-array 0 :adjustable t :fill-pointer 0))
(p-defcell mro::$VERSION (make-p-box nil))

(p-defcell $VERSION (make-p-box nil))

(p-eval-always (p-note-inc "strict"))

(p-eval-always (p-note-inc "warnings"))

(p-sub pl-import
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-return-empty))))

(p-sub pl-unimport
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-return-empty))))

(p-sub pl-get_linear_isa
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-raw-params (($class :scalar) ($type :scalar))
    (block nil (p-tail-value (mro::pl-_c3_linearize $class)))))

(p-sub pl-get_mro
  (&rest %_args)
  (:returns :str :wantarray-insensitive t :writes-args nil :needs ())
  (p-args-body (block nil (p-tail-value "c3"))))

(p-sub pl-set_mro
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-return-empty))))

(p-sub pl-get_isarev
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body
    (block nil (p-tail-value (make-p-box (make-array 0 :adjustable t :fill-pointer 0))))))

(p-sub pl-is_universal
  (&rest %_args)
  (:returns :num :wantarray-insensitive t :writes-args nil :needs (:nonlocal_exit.return))
  (p-raw-params (($class :scalar))
    (block nil
      (p-void-ctx (p-if (p-str-eq $class "UNIVERSAL") (p-return 1))
        (p-foreach-raw ($u (p-cast-@ "UNIVERSAL::ISA" (p-symref-site)))
          :my
          t
          (p-if (p-str-eq $u $class) (p-return 1)))
        (p-caller-ctx (p-tail-value 0))))))

(p-eval-always (p-note-inc "strict"))

(p-sub pl-invalidate_all_method_caches
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-return-empty))))

(p-sub pl-method_changed_in
  (&rest %_args)
  (:writes-args nil :needs ())
  (p-args-body (block nil (p-return-empty))))

(p-sub pl-_c3_linearize
  (&rest %_args)
  (:writes-args nil
    :needs (:nonlocal_exit.die :nonlocal_exit.loop_control :nonlocal_exit.return))
  (p-args-body
    (block nil
      (p-let (($class :box (make-p-box nil)) ($seen :box (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $class $seen) @_))
        (p-void-ctx
          (p-my-= $seen
            (make-p-box (p-hash (p-cast-% (p-|| $seen (make-p-box (p-hash))) :rvalue))))
          (p-if (p-post++ (p-gethash-deref-box $seen $class))
            (progn
              (p-die :loc
                "lib/mro.pm line 75"
                (p-string-concat "Recursive inheritance detected in package '"
                  $class
                  (p-esc "'\\n")))))
          (p-let ((@parents :array (make-array 0 :adjustable t :fill-pointer 0)))
            (p-array-= @parents (p-cast-@ (p-string-concat $class "::ISA") :rvalue))
            (p-if (p-! @parents) (p-return (make-p-box (p-array-init $class))))
            (p-let ((@seqs :array (make-array 0 :adjustable t :fill-pointer 0)))
              (p-array-= @seqs
                (p-map
                  (lambda ($_)
                    (make-p-box
                      (p-array-init
                        (p-cast-@ (p-scalar-ctx (mro::pl-_c3_linearize $_ $seen)) :rvalue))))
                  @parents))
              (p-push @seqs (make-p-box (p-array-init @parents)))
              (p-let ((@result :array (make-array 0 :adjustable t :fill-pointer 0)))
                (p-array-= @result (vector $class))
                (p-while 1
                  (p-array-= @seqs
                    (p-grep (lambda ($_) (p-scalar (p-cast-@ $_ :rvalue))) @seqs))
                  (p-if (p-! @seqs) (p-last))
                  (p-let (($cand :box (make-p-box nil)))
                    (p-foreach ($seq @seqs)
                      :my
                      t
                      (p-let (($head :box (make-p-box nil)))
                        (p-my-= $head (p-aref-deref $seq 0))
                        (p-let (($in_tail :scalar 0))
                          (p-foreach ($s @seqs)
                            :my
                            t
                            (p-foreach-range-raw ($i 1 (p-array-last-index $s))
                              :my
                              t
                              (p-if (p-str-eq (p-aref-deref $s $i) $head)
                                (progn (setf $in_tail 1) (p-last))))
                            (p-if $in_tail (p-last)))
                          (p-if (p-! $in_tail) (progn (p-my-= $cand $head) (p-last))))))
                    (p-if (p-! (p-defined $cand))
                      (p-die :loc
                        "lib/mro.pm line 105"
                        (p-string-concat "Inconsistent hierarchy during C3 merge of '"
                          $class
                          (p-esc "'\\n"))))
                    (p-push @result $cand)
                    (p-foreach ($seq @seqs)
                      :my
                      t
                      (p-if
                        (p-&& (p-cast-@ $seq :rvalue) (p-str-eq (p-aref-deref $seq 0) $cand))
                        (p-shift (p-cast-@ $seq))))))
                (p-caller-ctx (p-tail-value (p-backslash @result)))))))))))

(p-eval-always (p-note-inc "strict"))

(pcl::p-eval-always (pcl::p-detect-end "lib/mro.pm"))
(p-run-compile-phase-blocks)

(p-set-current-package :mro "mro")

(p-scalar-= mro::$VERSION "1.29_01")

1

