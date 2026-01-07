(in-package :pcl)

;; use v5.32
;; (include handling not yet implemented)

;; use strict
;; (include handling not yet implemented)

;; use warnings
;; (include handling not yet implemented)

;; use feature 'signatures'
;; (include handling not yet implemented)

;; no warnings 'experimental::signatures'
;; (include handling not yet implemented)

;; say "=== Test 07: Objects ==="
(pl-say "=== Test 07: Objects ===")

;; say ""
(pl-say "")

;; say "--- bless ---"
(pl-say "--- bless ---")

;; my $obj = {name => "Widget", value => 42}
(pl-setf $obj (pl-hash "name" "Widget" "value" 42))

;; bless $obj, "Thing"
(pl-bless $obj "Thing")

;; say "ref=" . ref($obj)
(pl-say (pl-. "ref=" (let ((*wantarray* t)) (pl-ref $obj))))

;; say "name=" . $obj->{name}
(pl-say (pl-. "name=" (pl-gethash-deref $obj "name")))

;; say "value=" . $obj->{value}
(pl-say (pl-. "value=" (pl-gethash-deref $obj "value")))

;; say ""
(pl-say "")

;; say "--- Counter class ---"
(pl-say "--- Counter class ---")

;;; package Counter
;; sub new { ... }
(defun pl-new (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $class = shift
            (pl-setf $class (pl-shift @_))
      
      ;; my $start = shift
            (pl-setf $start (pl-shift @_))
      
      ;; $start = 0 unless defined $start
      (pl-unless       (pl-defined $start)       (pl-setf $start 0))
      
      ;; my $self = {count => $start}
            (pl-setf $self (pl-hash "count" $start))
      
      ;; bless $self, $class
            (pl-bless $self $class)
      
      ;; return $self
            (pl-return $self)
      
    )
  )
)

;; sub increment { ... }
(defun pl-increment (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; $self->{count} = $self->{count} + 1
            (pl-setf (pl-gethash-deref $self "count") (pl-+ (pl-gethash-deref $self "count") 1))
      
    )
  )
)

;; sub decrement { ... }
(defun pl-decrement (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; $self->{count} = $self->{count} - 1
            (pl-setf (pl-gethash-deref $self "count") (pl-- (pl-gethash-deref $self "count") 1))
      
    )
  )
)

;; sub get { ... }
(defun pl-get (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; return $self->{count}
            (pl-return (pl-gethash-deref $self "count"))
      
    )
  )
)

;; sub add { ... }
(defun pl-add (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; my $n = shift
            (pl-setf $n (pl-shift @_))
      
      ;; $self->{count} = $self->{count} + $n
            (pl-setf (pl-gethash-deref $self "count") (pl-+ (pl-gethash-deref $self "count") $n))
      
    )
  )
)

;;; end package Counter

;;; package main

;; my $c = Counter->new(0)
(pl-setf $c (pl-method-call "Counter" 'new 0))

;; say "initial=" . $c->get()
(pl-say (pl-. "initial=" (pl-method-call $c 'get)))

;; $c->increment()
(pl-method-call $c 'increment)

;; say "after incr=" . $c->get()
(pl-say (pl-. "after incr=" (pl-method-call $c 'get)))

;; $c->increment()
(pl-method-call $c 'increment)

;; $c->increment()
(pl-method-call $c 'increment)

;; say "after 2 more=" . $c->get()
(pl-say (pl-. "after 2 more=" (pl-method-call $c 'get)))

;; $c->decrement()
(pl-method-call $c 'decrement)

;; say "after decr=" . $c->get()
(pl-say (pl-. "after decr=" (pl-method-call $c 'get)))

;; $c->add(10)
(pl-method-call $c 'add 10)

;; say "after add(10)=" . $c->get()
(pl-say (pl-. "after add(10)=" (pl-method-call $c 'get)))

;; say "ref(c)=" . ref($c)
(pl-say (pl-. "ref(c)=" (let ((*wantarray* t)) (pl-ref $c))))

;; say ""
(pl-say "")

;; say "--- Point class ---"
(pl-say "--- Point class ---")

;;; package Point
;; sub new { ... }
(defun pl-new (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $class = shift
            (pl-setf $class (pl-shift @_))
      
      ;; my $x = shift
            (pl-setf $x (pl-shift @_))
      
      ;; my $y = shift
            (pl-setf $y (pl-shift @_))
      
      ;; $x = 0 unless defined $x
      (pl-unless       (pl-defined $x)       (pl-setf $x 0))
      
      ;; $y = 0 unless defined $y
      (pl-unless       (pl-defined $y)       (pl-setf $y 0))
      
      ;; my $self = {x => $x, y => $y}
            (pl-setf $self (pl-hash "x" $x "y" $y))
      
      ;; bless $self, $class
            (pl-bless $self $class)
      
      ;; return $self
            (pl-return $self)
      
    )
  )
)

;; sub x { ... }
(defun pl-x (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; return $self->{x}
            (pl-return (pl-gethash-deref $self "x"))
      
    )
  )
)

;; sub y { ... }
(defun pl-y (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; return $self->{y}
            (pl-return (pl-gethash-deref $self "y"))
      
    )
  )
)

;; sub move { ... }
(defun pl-move (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; my $dx = shift
            (pl-setf $dx (pl-shift @_))
      
      ;; my $dy = shift
            (pl-setf $dy (pl-shift @_))
      
      ;; $self->{x} = $self->{x} + $dx
            (pl-setf (pl-gethash-deref $self "x") (pl-+ (pl-gethash-deref $self "x") $dx))
      
      ;; $self->{y} = $self->{y} + $dy
            (pl-setf (pl-gethash-deref $self "y") (pl-+ (pl-gethash-deref $self "y") $dy))
      
    )
  )
)

;; sub to_string { ... }
(defun pl-to_string (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; return "(" . $self->{x} . "," . $self->{y} . ")"
            (pl-return (pl-. (pl-. (pl-. (pl-. "(" (pl-gethash-deref $self "x")) ",") (pl-gethash-deref $self "y")) ")"))
      
    )
  )
)

;;; end package Point

;;; package main

;; my $p = Point->new(10, 20)
(pl-setf $p (pl-method-call "Point" 'new 10 20))

;; say "point=" . $p->to_string()
(pl-say (pl-. "point=" (pl-method-call $p 'to_string)))

;; say "x=" . $p->x()
(pl-say (pl-. "x=" (pl-method-call $p 'x)))

;; say "y=" . $p->y()
(pl-say (pl-. "y=" (pl-method-call $p 'y)))

;; $p->move(5, -3)
(pl-method-call $p 'move 5 -3)

;; say "after move(5,-3)=" . $p->to_string()
(pl-say (pl-. "after move(5,-3)=" (pl-method-call $p 'to_string)))

;; say "ref(p)=" . ref($p)
(pl-say (pl-. "ref(p)=" (let ((*wantarray* t)) (pl-ref $p))))

;; say ""
(pl-say "")

;; say "--- multiple instances ---"
(pl-say "--- multiple instances ---")

;; my $p1 = Point->new(0, 0)
(pl-setf $p1 (pl-method-call "Point" 'new 0 0))

;; my $p2 = Point->new(100, 100)
(pl-setf $p2 (pl-method-call "Point" 'new 100 100))

;; say "p1=" . $p1->to_string()
(pl-say (pl-. "p1=" (pl-method-call $p1 'to_string)))

;; say "p2=" . $p2->to_string()
(pl-say (pl-. "p2=" (pl-method-call $p2 'to_string)))

;; $p1->move(10, 10)
(pl-method-call $p1 'move 10 10)

;; say "p1 after move=" . $p1->to_string()
(pl-say (pl-. "p1 after move=" (pl-method-call $p1 'to_string)))

;; say "p2 unchanged=" . $p2->to_string()
(pl-say (pl-. "p2 unchanged=" (pl-method-call $p2 'to_string)))

;; say ""
(pl-say "")

;; say "=== Done ==="
(pl-say "=== Done ===")

