(in-package :pcl)

;; use strict
;; (include handling not yet implemented)

;; use warnings
;; (include handling not yet implemented)

;; sub context_sub { ... }
(defun pl-context_sub (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; if (wantarray()) {         return (1, 2, 3);     } else {         return "scalar";     }
      ;; if (wantarray())
      (pl-if       (pl-wantarray)
        (progn
          ;; return (1, 2, 3)
                    (pl-return 1 2 3)
          
        )
        ;; else
        (progn
          ;; return "scalar"
                    (pl-return "scalar")
          
        )
      )
      
    )
  )
)

;;; package Ctx

;; sub new { ... }
(defun pl-new (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $class = shift
            (pl-setf $class (pl-shift @_))
      
      ;; my $self = {}
            (pl-setf $self (pl-hash ))
      
      ;; bless $self, $class
            (pl-bless $self $class)
      
      ;; return $self
            (pl-return $self)
      
    )
  )
)

;; sub context_method { ... }
(defun pl-context_method (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; if (wantarray()) {         return (10, 20, 30);     } else {         return "method-scalar";     }
      ;; if (wantarray())
      (pl-if       (pl-wantarray)
        (progn
          ;; return (10, 20, 30)
                    (pl-return 10 20 30)
          
        )
        ;; else
        (progn
          ;; return "method-scalar"
                    (pl-return "method-scalar")
          
        )
      )
      
    )
  )
)

;;; package main

;; my $s = context_sub()
(pl-setf $s (pl-context_sub))

;; print "sub scalar: $s\n"
(pl-print (pl-string_concat "sub scalar: " $s "
"))

;; my @a = context_sub()
(pl-setf @a (let ((*wantarray* t)) (pl-context_sub)))

;; print "sub list: @a\n"
(pl-print (pl-string_concat "sub list: " @a "
"))

;; my $obj = Ctx->new()
(pl-setf $obj (pl-method-call "Ctx" 'new))

;; my $ms = $obj->context_method()
(pl-setf $ms (pl-method-call $obj 'context_method))

;; print "method scalar: $ms\n"
(pl-print (pl-string_concat "method scalar: " $ms "
"))

;; my @ma = $obj->context_method()
(pl-setf @ma (let ((*wantarray* t)) (pl-method-call $obj 'context_method)))

;; print "method list: @ma\n"
(pl-print (pl-string_concat "method list: " @ma "
"))

