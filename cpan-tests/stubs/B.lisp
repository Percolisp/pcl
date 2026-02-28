;;; Stub for Perl B module (introspection)
;;; YAML::Tiny uses B::svref_2object for checking variable types

(defpackage :B (:use :cl :pcl))
(in-package :B)

(defvar $VERSION (pcl::make-pl-box "1.80"))

;; B::svref_2object returns an object with FLAGS and other methods
;; We stub this with a minimal implementation
(defstruct b-sv
  (value nil)
  (flags 0))

(defun pl-svref_2object (ref)
  "Stub: return a B::SV-like object"
  (make-b-sv :value ref :flags 0))

;; FLAGS method - return some default flags
(defmethod pcl::pl-method-dispatch ((obj b-sv) method &rest args)
  (declare (ignore args))
  (case method
    (FLAGS (b-sv-flags obj))
    (otherwise 0)))
