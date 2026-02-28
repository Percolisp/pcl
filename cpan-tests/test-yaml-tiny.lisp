;;; Test loading YAML::Tiny with stubs

(in-package :pcl)

;; Mark core modules as already loaded (stubs)
(setf (gethash "B.pm" *pl-inc-table*) "stub")
(setf (gethash "Scalar/Util.pm" *pl-inc-table*) "stub")
(setf (gethash "Exporter.pm" *pl-inc-table*) "stub")
(setf (gethash "Fcntl.pm" *pl-inc-table*) "stub")
(setf (gethash "Config.pm" *pl-inc-table*) "stub")
(setf (gethash "utf8.pm" *pl-inc-table*) "stub")

;; Create minimal stub packages
(defpackage :B (:use :cl :pcl))
(in-package :B)
(defstruct b-sv (value nil) (flags 0))
(defun pl-svref_2object (ref) (make-b-sv :value ref :flags 0))

(defpackage :|Scalar::Util| (:use :cl :pcl))
(in-package :|Scalar::Util|)
(defvar $VERSION (pcl::make-pl-box "1.60"))
(defun pl-refaddr (ref) (sxhash ref))
(defun pl-looks_like_number (val)
  (handler-case
      (progn (read-from-string (pcl::to-string val)) t)
    (error () nil)))

(defpackage :Exporter (:use :cl :pcl))
(in-package :Exporter)
;; CLOS class for inheritance
(defclass exporter () ())

(defpackage :Fcntl (:use :cl :pcl))
(in-package :Fcntl)
(defun pl-LOCK_SH () 1)
(defun pl-LOCK_EX () 2)
(defun pl-LOCK_UN () 8)

(defpackage :Config (:use :cl :pcl))
(defpackage :utf8 (:use :cl :pcl))

(in-package :pcl)

;; Debug: check what's in the hash
(format t "~%Hash contents before loading:~%")
(maphash #'(lambda (k v) (format t "  ~A => ~A~%" k v)) *pl-inc-table*)

(format t "~%Stubs loaded. Loading YAML::Tiny...~%")

;; Load transpiled YAML::Tiny
(load "cpan-tests/transpiled/yaml-tiny.lisp")

(format t "~%YAML::Tiny loaded successfully!~%")
