;;; Stub for Perl Scalar::Util module
;;; YAML::Tiny uses refaddr

(defpackage :|Scalar::Util| (:use :cl :pcl))
(in-package :|Scalar::Util|)

(defvar $VERSION (pcl::make-pl-box "1.60"))

;; refaddr returns the memory address of the referent
;; In CL, we can use sxhash or object identity
(defun pl-refaddr (ref)
  "Return a unique address for the reference"
  (sxhash ref))

;; looks_like_number - check if value looks like a number
(defun pl-looks_like_number (val)
  "Check if value looks like a number"
  (let ((s (pcl::to-string val)))
    (handler-case
        (progn
          (parse-number:parse-number s)
          t)
      (error () nil))))

;; blessed - return class name if blessed, undef otherwise
(defun pl-blessed (ref)
  "Return class name if ref is blessed"
  (if (typep ref 'pcl::pl-blessed-ref)
      (pcl::pl-blessed-ref-class ref)
      pcl::*pl-undef*))
