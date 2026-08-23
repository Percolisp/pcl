;;; pcl: pipeline=v2 gen=v2-201
;;;; Copyright (c) 2025-2026 the PCL authors
;;;; This is free software; you can redistribute it and/or modify it under the
;;;; same terms as the Perl 5 programming language system itself.
;;;; SPDX-License-Identifier: Artistic-1.0-Perl OR GPL-1.0-or-later

(in-package :pcl)
(p-defpackage :main)
(in-package :main)

(pcl:p-defpackage :warnings)

(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))



;;; package warnings
(p-defpackage :warnings)
(in-package :warnings)
(defclass plc-warnings () ())
(p-register-pkg-name :warnings "warnings")

(p-declare-sub pl-import)
(p-declare-sub pl-unimport)
(p-declare-sub pl-enabled)
(p-declare-sub pl-fatal_enabled)
(p-declare-sub pl-enabled_at_level)
(p-declare-sub pl-fatal_enabled_at_level)
(p-declare-sub pl-register_categories)
(p-declare-sub pl-warn)
(p-declare-sub pl-warnif)
(defvar $a (make-p-box nil))
(defvar $b (make-p-box nil))

;; Forward declarations for undeclared package globals
(p-defcell $message (make-p-box nil))
(p-defcell warnings::$VERSION (make-p-box nil))

(p-defcell $VERSION (make-p-box nil))

(p-sub pl-import (&rest %_args) (p-args-body (block nil)))

(p-sub pl-unimport (&rest %_args) (p-args-body (block nil)))

(p-sub pl-enabled (&rest %_args) (p-args-body (block nil (p-return 1))))

(p-sub pl-fatal_enabled (&rest %_args) (p-args-body (block nil (p-return 0))))

(p-sub pl-enabled_at_level (&rest %_args) (p-args-body (block nil (p-return 1))))

(p-sub pl-fatal_enabled_at_level (&rest %_args) (p-args-body (block nil (p-return 0))))

(p-sub pl-register_categories (&rest %_args) (p-args-body (block nil)))

(p-sub pl-warn
  (&rest %_args)
  (p-args-body
    (block nil
      (p-void-ctx
        (let (($message (make-p-box nil)))
          (p-my-= $message (p-pop @_))
          (p-caller-ctx (p-warn :loc "lib/warnings.pm line 38" $message)))))))

(p-sub pl-warnif
  (&rest %_args)
  (p-args-body
    (block nil
      (p-void-ctx
        (let (($message (make-p-box nil)))
          (p-my-= $message (p-pop @_))
          (p-caller-ctx (p-warn :loc "lib/warnings.pm line 44" $message)))))))

(p-run-compile-phase-blocks)

(p-set-current-package :warnings "warnings")

(p-scalar-= warnings::$VERSION "1.70")

1

