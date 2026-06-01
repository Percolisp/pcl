;;;; Copyright (c) 2025-2026
;;;; This is free software; you can redistribute it and/or modify it
;;;; under the same terms as the Perl 5 programming language system itself.

(in-package :pcl)

;; Test simple return
(defun pl-test-return (&key wantarray)
  (block nil
    (pl-return 42)))

(format t "test-return: ~A~%" (pl-test-return))

;; Test conditional return
(defun pl-abs ($x &key wantarray)
  (block nil
    (if (pl-< $x 0)
        (pl-return (pl-- 0 $x)))
    $x))

(format t "abs(-5): ~A~%" (pl-abs -5))
(format t "abs(10): ~A~%" (pl-abs 10))

(format t "~%All sub tests passed!~%")
