;;; pcl: pipeline=v2 gen=v2-340
;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

(in-package :pcl)
(p-defpackage :main)
(in-package :main)

(pcl:p-defpackage :mro)

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



;;; package mro
(p-defpackage :mro)
(in-package :mro)
(defclass plc-mro () ())
(p-register-pkg-name :mro "mro")

(p-declare-sub pl-import)
(p-declare-sub pl-unimport)
(p-declare-sub pl-get_linear_isa)
(p-declare-sub pl-get_mro)
(p-declare-sub pl-set_mro)
(p-declare-sub pl-get_isarev)
(p-declare-sub pl-is_universal)
(p-declare-sub pl-invalidate_all_method_caches)
(p-declare-sub pl-method_changed_in)
(p-declare-sub pl-_c3_linearize)
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

(p-sub pl-import (&rest %_args) (p-args-body (block nil)))

(p-sub pl-unimport (&rest %_args) (p-args-body (block nil)))

(p-sub pl-get_linear_isa
  (&rest %_args)
  (p-raw-params ($class $type) (block nil (p-return (mro::pl-_c3_linearize $class)))))

(p-sub pl-get_mro (&rest %_args) (p-args-body (block nil (p-return "c3"))))

(p-sub pl-set_mro (&rest %_args) (p-args-body (block nil (p-return))))

(p-sub pl-get_isarev
  (&rest %_args)
  (p-args-body (block nil (p-return (make-p-box (make-array 0 :adjustable t :fill-pointer 0))))))

(p-sub pl-is_universal
  (&rest %_args)
  (p-raw-params ($class)
    (block nil
      (p-void-ctx (p-if (p-str-eq $class "UNIVERSAL") (p-return 1))
        (p-foreach ($u (p-cast-@ "UNIVERSAL::ISA"))
          :my
          t
          (p-if (p-str-eq $u $class) (p-return 1)))
        (p-return 0)))))

(p-eval-always (p-note-inc "strict"))

(p-sub pl-invalidate_all_method_caches (&rest %_args) (p-args-body (block nil (p-return))))

(p-sub pl-method_changed_in (&rest %_args) (p-args-body (block nil (p-return))))

(p-sub pl-_c3_linearize
  (&rest %_args)
  (p-args-body
    (block nil
      (let (($class (make-p-box nil)) ($seen (make-p-box nil)))
        (p-scalar-ctx (p-list-= (vector $class $seen) @_))
        (p-void-ctx
          (p-my-= $seen (make-p-box (p-hash (p-cast-% (p-|| $seen (make-p-box (p-hash)))))))
          (p-if (p-post++ (p-gethash-deref-box $seen $class))
            (progn
              (p-die :loc
                "lib/mro.pm line 71"
                (p-string-concat "Recursive inheritance detected in package '"
                  $class
                  "'
"))))
          (let ((@parents (make-array 0 :adjustable t :fill-pointer 0)))
            (p-array-= @parents (p-cast-@ (p-string-concat $class "::ISA")))
            (p-if (p-! @parents) (p-return (make-p-box (p-array-init $class))))
            (let ((@seqs (make-array 0 :adjustable t :fill-pointer 0)))
              (p-array-= @seqs
                (p-list-ctx
                  (p-map
                    (lambda ($_)
                      (make-p-box
                        (p-array-init (p-cast-@ (p-list-ctx (mro::pl-_c3_linearize $_ $seen))))))
                    @parents)))
              (p-push @seqs (make-p-box (p-array-init @parents)))
              (let ((@result (make-array 0 :adjustable t :fill-pointer 0)))
                (p-array-= @result (vector $class))
                (p-while 1
                  (p-array-= @seqs
                    (p-list-ctx (p-grep (lambda ($_) (p-scalar (p-cast-@ $_))) @seqs)))
                  (p-if (p-! @seqs) (p-last))
                  (let (($cand (make-p-box nil)))
                    (p-foreach ($seq @seqs)
                      :my
                      t
                      (let (($head (make-p-box nil)))
                        (p-my-= $head (p-aref-deref $seq 0))
                        (let (($in_tail 0))
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
                        "lib/mro.pm line 101"
                        (p-string-concat "Inconsistent hierarchy during C3 merge of '"
                          $class
                          "'
")))
                    (p-push @result $cand)
                    (p-foreach ($seq @seqs)
                      :my
                      t
                      (p-if (p-&& (p-cast-@ $seq) (p-str-eq (p-aref-deref $seq 0) $cand))
                        (p-shift (p-cast-@ $seq))))))
                (p-return (p-backslash @result))))))))))

(p-eval-always (p-note-inc "strict"))

(p-run-compile-phase-blocks)

(p-set-current-package :mro "mro")

(p-scalar-= mro::$VERSION "1.29_01")

1

