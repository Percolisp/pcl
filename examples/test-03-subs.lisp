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

;; say "=== Test 03: Subroutines ==="
(pl-say "=== Test 03: Subroutines ===")

;; say ""
(pl-say "")

;; say "--- no params ---"
(pl-say "--- no params ---")

;; sub greet() { ... }
(defun pl-greet (&rest @_)
  (block nil
    ;; say "Hello!"
        (pl-say "Hello!")
    
  )
)

;; greet()
(pl-greet)

;; sub get_magic() { ... }
(defun pl-get_magic (&rest @_)
  (block nil
    ;; return 42
        (pl-return 42)
    
  )
)

;; my $magic = get_magic()
(pl-setf $magic (pl-get_magic))

;; say "magic=" . $magic
(pl-say (pl-. "magic=" $magic))

;; say ""
(pl-say "")

;; say "--- signature ---"
(pl-say "--- signature ---")

;; sub add($a, $b) { ... }
(defun pl-add ($a $b)
  (block nil
    ;; return $a + $b
        (pl-return (pl-+ $a $b))
    
  )
)

;; my $sum = add(10, 20)
(pl-setf $sum (pl-add 10 20))

;; say "10+20=" . $sum
(pl-say (pl-. "10+20=" $sum))

;; sub multiply($x, $y) { ... }
(defun pl-multiply ($x $y)
  (block nil
    ;; return $x * $y
        (pl-return (pl-* $x $y))
    
  )
)

;; my $prod = multiply(6, 7)
(pl-setf $prod (pl-multiply 6 7))

;; say "6*7=" . $prod
(pl-say (pl-. "6*7=" $prod))

;; sub subtract($a, $b) { ... }
(defun pl-subtract ($a $b)
  (block nil
    ;; return $a - $b
        (pl-return (pl-- $a $b))
    
  )
)

;; say "100-37=" . subtract(100, 37)
(pl-say (pl-. "100-37=" (let ((*wantarray* t)) (pl-subtract 100 37))))

;; say ""
(pl-say "")

;; say "--- defaults ---"
(pl-say "--- defaults ---")

;; sub greet_default($name = "World") { ... }
(defun pl-greet_default ( &optional ($name "World"))
  (block nil
    ;; say "Hi, " . $name . "!"
        (pl-say (pl-. (pl-. "Hi, " $name) "!"))
    
  )
)

;; greet_default("Charlie")
(pl-greet_default "Charlie")

;; greet_default()
(pl-greet_default)

;; sub power($base, $exp = 2) { ... }
(defun pl-power ($base &optional ($exp 2))
  (block nil
    ;; my $result = 1
        (pl-setf $result 1)
    
    ;; for (my $i = 0; $i < $exp; $i++) {         $result *= $base;     }
    (pl-for (    (pl-setf $i 0))
            (    (pl-< $i $exp))
            (    (pl-++-post $i))
      ;; $result *= $base
            (pl-*= $result $base)
      
    )
    
    ;; return $result
        (pl-return $result)
    
  )
)

;; say "3^2=" . power(3)
(pl-say (pl-. "3^2=" (let ((*wantarray* t)) (pl-power 3))))

;; say "2^4=" . power(2, 4)
(pl-say (pl-. "2^4=" (let ((*wantarray* t)) (pl-power 2 4))))

;; say ""
(pl-say "")

;; say "--- implicit return ---"
(pl-say "--- implicit return ---")

;; sub double($n) { ... }
(defun pl-double ($n)
  (block nil
    ;; $n * 2
        (pl-* $n 2)
    
  )
)

;; say "double(21)=" . double(21)
(pl-say (pl-. "double(21)=" (let ((*wantarray* t)) (pl-double 21))))

;; sub max($a, $b) { ... }
(defun pl-max ($a $b)
  (block nil
    ;; $a > $b ? $a : $b
        (pl-if (pl-> $a $b) $a $b)
    
  )
)

;; say "max(15,22)=" . max(15, 22)
(pl-say (pl-. "max(15,22)=" (let ((*wantarray* t)) (pl-max 15 22))))

;; say ""
(pl-say "")

;; say "--- recursion ---"
(pl-say "--- recursion ---")

;; sub factorial($n) { ... }
(defun pl-factorial ($n)
  (block nil
    ;; if ($n <= 1) {         return 1;     }
    ;; if ($n <= 1)
    (pl-if     (pl-<= $n 1)
      (progn
        ;; return 1
                (pl-return 1)
        
      )
      nil
    )
    
    ;; return $n * factorial($n - 1)
        (pl-return (pl-* $n (pl-factorial (pl-- $n 1))))
    
  )
)

;; say "5!=" . factorial(5)
(pl-say (pl-. "5!=" (let ((*wantarray* t)) (pl-factorial 5))))

;; say "6!=" . factorial(6)
(pl-say (pl-. "6!=" (let ((*wantarray* t)) (pl-factorial 6))))

;; sub fib($n) { ... }
(defun pl-fib ($n)
  (block nil
    ;; if ($n <= 1) {         return $n;     }
    ;; if ($n <= 1)
    (pl-if     (pl-<= $n 1)
      (progn
        ;; return $n
                (pl-return $n)
        
      )
      nil
    )
    
    ;; return fib($n - 1) + fib($n - 2)
        (pl-return (pl-+ (pl-fib (pl-- $n 1)) (pl-fib (pl-- $n 2))))
    
  )
)

;; say "fib(7)=" . fib(7)
(pl-say (pl-. "fib(7)=" (let ((*wantarray* t)) (pl-fib 7))))

;; say "fib(10)=" . fib(10)
(pl-say (pl-. "fib(10)=" (let ((*wantarray* t)) (pl-fib 10))))

;; say ""
(pl-say "")

;; say "--- sub calls sub ---"
(pl-say "--- sub calls sub ---")

;; sub square($n) { ... }
(defun pl-square ($n)
  (block nil
    ;; return $n * $n
        (pl-return (pl-* $n $n))
    
  )
)

;; sub sum_of_squares($a, $b) { ... }
(defun pl-sum_of_squares ($a $b)
  (block nil
    ;; return square($a) + square($b)
        (pl-return (pl-+ (pl-square $a) (pl-square $b)))
    
  )
)

;; say "3^2+4^2=" . sum_of_squares(3, 4)
(pl-say (pl-. "3^2+4^2=" (let ((*wantarray* t)) (pl-sum_of_squares 3 4))))

;; say ""
(pl-say "")

;; say "--- many params ---"
(pl-say "--- many params ---")

;; sub sum5($a, $b, $c, $d, $e) { ... }
(defun pl-sum5 ($a $b $c $d $e)
  (block nil
    ;; return $a + $b + $c + $d + $e
        (pl-return (pl-+ (pl-+ (pl-+ (pl-+ $a $b) $c) $d) $e))
    
  )
)

;; say "1+2+3+4+5=" . sum5(1, 2, 3, 4, 5)
(pl-say (pl-. "1+2+3+4+5=" (let ((*wantarray* t)) (pl-sum5 1 2 3 4 5))))

;; say ""
(pl-say "")

;; say "=== Done ==="
(pl-say "=== Done ===")

