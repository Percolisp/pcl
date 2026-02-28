;;; Stub for Perl's warnings pragma
;;; Just defines the $VERSION so Carp.pm can load

(defpackage :warnings (:use :cl :pcl))
(in-package :warnings)

;; warnings $VERSION - Carp checks this
(defvar $VERSION (pcl::make-pl-box "1.50"))

;; Stub functions
(defun pl-import (&rest args)
  (declare (ignore args))
  nil)

(defun pl-unimport (&rest args)
  (declare (ignore args))
  nil)

(in-package :pcl)
