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

;; say "=== PCL Combined Test ==="
(pl-say "=== PCL Combined Test ===")

;; say ""
(pl-say "")

;; say "--- arithmetic ---"
(pl-say "--- arithmetic ---")

;; my $a = 10
(pl-setf $a 10)

;; my $b = 3
(pl-setf $b 3)

;; say "a=$a b=$b"
(pl-say (pl-string_concat "a=" $a " b=" $b))

;; say "a+b=" . ($a + $b)
(pl-say (pl-. "a+b=" (pl-+ $a $b)))

;; say "a-b=" . ($a - $b)
(pl-say (pl-. "a-b=" (pl-- $a $b)))

;; say "a*b=" . ($a * $b)
(pl-say (pl-. "a*b=" (pl-* $a $b)))

;; say "a/b=" . ($a / $b)
(pl-say (pl-. "a/b=" (pl-/ $a $b)))

;; say "a%b=" . ($a % $b)
(pl-say (pl-. "a%b=" (pl-% $a $b)))

;; say "a**b=" . ($a ** $b)
(pl-say (pl-. "a**b=" (pl-** $a $b)))

;; say ""
(pl-say "")

;; say "--- comparison ---"
(pl-say "--- comparison ---")

;; say "10>5=" . (10 > 5 ? 1 : 0)
(pl-say (pl-. "10>5=" (pl-if (pl-> 10 5) 1 0)))

;; say "10<5=" . (10 < 5 ? 1 : 0)
(pl-say (pl-. "10<5=" (pl-if (pl-< 10 5) 1 0)))

;; say "10==10=" . (10 == 10 ? 1 : 0)
(pl-say (pl-. "10==10=" (pl-if (pl-== 10 10) 1 0)))

;; say "10!=5=" . (10 != 5 ? 1 : 0)
(pl-say (pl-. "10!=5=" (pl-if (pl-!= 10 5) 1 0)))

;; say ""
(pl-say "")

;; say "--- logical ---"
(pl-say "--- logical ---")

;; say "1&&1=" . (1 && 1)
(pl-say (pl-. "1&&1=" (pl-&& 1 1)))

;; say "1&&0=" . (1 && 0)
(pl-say (pl-. "1&&0=" (pl-&& 1 0)))

;; say "0||1=" . (0 || 1)
(pl-say (pl-. "0||1=" (pl-|| 0 1)))

;; say "!0=" . (!0 ? 1 : 0)
(pl-say (pl-. "!0=" (pl-if (pl-! 0) 1 0)))

;; say ""
(pl-say "")

;; say "--- string ops ---"
(pl-say "--- string ops ---")

;; my $s1 = "hello"
(pl-setf $s1 "hello")

;; my $s2 = "world"
(pl-setf $s2 "world")

;; say "concat=" . $s1 . " " . $s2
(pl-say (pl-. (pl-. (pl-. "concat=" $s1) " ") $s2))

;; say "length=" . length($s1)
(pl-say (pl-. "length=" (let ((*wantarray* t)) (pl-length $s1))))

;; say "uc=" . uc($s1)
(pl-say (pl-. "uc=" (let ((*wantarray* t)) (pl-uc $s1))))

;; say "lc=" . uc($s1)
(pl-say (pl-. "lc=" (let ((*wantarray* t)) (pl-uc $s1))))

;; say "substr=" . substr($s1, 1, 3)
(pl-say (pl-. "substr=" (let ((*wantarray* t)) (pl-substr $s1 1 3))))

;; say ""
(pl-say "")

;; say "--- increment/decrement ---"
(pl-say "--- increment/decrement ---")

;; my $n = 5
(pl-setf $n 5)

;; say "n=$n"
(pl-say (pl-string_concat "n=" $n))

;; $n++
(pl-++-post $n)

;; say "n++=$n"
(pl-say (pl-string_concat "n++=" $n))

;; $n--
(pl----post $n)

;; say "n--=$n"
(pl-say (pl-string_concat "n--=" $n))

;; $n += 10
(pl-incf $n 10)

;; say "n+=10=$n"
(pl-say (pl-string_concat "n+=10=" $n))

;; $n -= 3
(pl-decf $n 3)

;; say "n-=3=$n"
(pl-say (pl-string_concat "n-=3=" $n))

;; say ""
(pl-say "")

;; say "--- if/else ---"
(pl-say "--- if/else ---")

;; my $x = 10
(pl-setf $x 10)

;; if ($x > 5) {     say "x>5"; }
;; if ($x > 5)
(pl-if (pl-> $x 5)
  (progn
    ;; say "x>5"
        (pl-say "x>5")
    
  )
  nil
)

;; if ($x > 20) {     say "x>20"; } else {     say "x<=20"; }
;; if ($x > 20)
(pl-if (pl-> $x 20)
  (progn
    ;; say "x>20"
        (pl-say "x>20")
    
  )
  ;; else
  (progn
    ;; say "x<=20"
        (pl-say "x<=20")
    
  )
)

;; if ($x < 5) {     say "x<5"; } elsif ($x < 15) {     say "5<=x<15"; } else {     say "x>=15"; }
;; if ($x < 5)
(pl-if (pl-< $x 5)
  (progn
    ;; say "x<5"
        (pl-say "x<5")
    
  )
  ;; elsif ($x < 15)
  (pl-if   (pl-< $x 15)
    (progn
      ;; say "5<=x<15"
            (pl-say "5<=x<15")
      
    )
    ;; else
    (progn
      ;; say "x>=15"
            (pl-say "x>=15")
      
    )
  )
)

;; say ""
(pl-say "")

;; say "--- while ---"
(pl-say "--- while ---")

;; my $i = 0
(pl-setf $i 0)

;; while ($i < 3) {     say "i=$i";     $i++; }
(pl-while (pl-< $i 3)
  ;; say "i=$i"
    (pl-say (pl-string_concat "i=" $i))
  
  ;; $i++
    (pl-++-post $i)
  
)

;; say ""
(pl-say "")

;; say "--- for ---"
(pl-say "--- for ---")

;; for (my $j = 0; $j < 3; $j++) {     say "j=$j"; }
(pl-for ((pl-setf $j 0))
        ((pl-< $j 3))
        ((pl-++-post $j))
  ;; say "j=$j"
    (pl-say (pl-string_concat "j=" $j))
  
)

;; say ""
(pl-say "")

;; say "--- foreach ---"
(pl-say "--- foreach ---")

;; my @items (bare declaration)

;; push @items, 10
(pl-push @items 10)

;; push @items, 20
(pl-push @items 20)

;; push @items, 30
(pl-push @items 30)

;; foreach my $item (@items) {     say "item=$item"; }
(pl-foreach ($item @items)
  ;; say "item=$item"
    (pl-say (pl-string_concat "item=" $item))
  
)

;; say ""
(pl-say "")

;; say "--- subs ---"
(pl-say "--- subs ---")

;; sub add($x, $y) { ... }
(defun pl-add ($x $y)
  (block nil
    ;; return $x + $y
        (pl-return (pl-+ $x $y))
    
  )
)

;; sub greet($name) { ... }
(defun pl-greet ($name)
  (block nil
    ;; return "Hello, $name!"
        (pl-return (pl-string_concat "Hello, " $name "!"))
    
  )
)

;; say "add(3,4)=" . add(3, 4)
(pl-say (pl-. "add(3,4)=" (let ((*wantarray* t)) (pl-add 3 4))))

;; say "greet=" . greet("World")
(pl-say (pl-. "greet=" (let ((*wantarray* t)) (pl-greet "World"))))

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

;; say ""
(pl-say "")

;; say "--- arrays ---"
(pl-say "--- arrays ---")

;; my @arr (bare declaration)

;; push @arr, 1
(pl-push @arr 1)

;; push @arr, 2
(pl-push @arr 2)

;; push @arr, 3
(pl-push @arr 3)

;; push @arr, 4
(pl-push @arr 4)

;; push @arr, 5
(pl-push @arr 5)

;; say "arr[0]=" . $arr[0]
(pl-say (pl-. "arr[0]=" (pl-aref @arr 0)))

;; say "arr[2]=" . $arr[2]
(pl-say (pl-. "arr[2]=" (pl-aref @arr 2)))

;; say "arr[-1]=" . $arr[-1]
(pl-say (pl-. "arr[-1]=" (pl-aref @arr -1)))

;; say "length=" . scalar(@arr)
(pl-say (pl-. "length=" (let ((*wantarray* t)) (pl-scalar @arr))))

;; push @arr, 6
(pl-push @arr 6)

;; say "after push, last=" . $arr[-1]
(pl-say (pl-. "after push, last=" (pl-aref @arr -1)))

;; my $popped = pop @arr
(pl-setf $popped (pl-pop @arr))

;; say "popped=$popped"
(pl-say (pl-string_concat "popped=" $popped))

;; my $shifted = shift @arr
(pl-setf $shifted (pl-shift @arr))

;; say "shifted=$shifted"
(pl-say (pl-string_concat "shifted=" $shifted))

;; unshift @arr, 0
(pl-unshift @arr 0)

;; say "after unshift, first=" . $arr[0]
(pl-say (pl-. "after unshift, first=" (pl-aref @arr 0)))

;; say ""
(pl-say "")

;; say "--- hashes ---"
(pl-say "--- hashes ---")

;; my %hash (bare declaration)

;; $hash{name} = "Alice"
(pl-setf (pl-gethash %hash "name") "Alice")

;; $hash{age} = 30
(pl-setf (pl-gethash %hash "age") 30)

;; say "name=" . $hash{name}
(pl-say (pl-. "name=" (pl-gethash %hash "name")))

;; say "age=" . $hash{age}
(pl-say (pl-. "age=" (pl-gethash %hash "age")))

;; $hash{city} = "NYC"
(pl-setf (pl-gethash %hash "city") "NYC")

;; say "city=" . $hash{city}
(pl-say (pl-. "city=" (pl-gethash %hash "city")))

;; say "keys=" . scalar(keys %hash)
(pl-say (pl-. "keys=" (let ((*wantarray* t)) (pl-scalar (let ((*wantarray* t)) (pl-keys %hash))))))

;; say ""
(pl-say "")

;; say "--- refs ---"
(pl-say "--- refs ---")

;; my $val = 42
(pl-setf $val 42)

;; my $ref = \$val
(pl-setf $ref (pl-backslash $val))

;; say "val=$val"
(pl-say (pl-string_concat "val=" $val))

;; say "deref=" . $$ref
(pl-say (pl-. "deref=" (pl-$ $ref)))

;; $$ref = 100
(pl-setf (pl-$ $ref) 100)

;; say "after mod, val=$val"
(pl-say (pl-string_concat "after mod, val=" $val))

;; my $aref = [1, 2, 3]
(pl-setf $aref (list 1 2 3))

;; say "aref->[0]=" . $aref->[0]
(pl-say (pl-. "aref->[0]=" (pl-aref-deref $aref 0)))

;; my $href = {x => 10, y => 20}
(pl-setf $href (pl-hash "x" 10 "y" 20))

;; say "href->{x}=" . $href->{x}
(pl-say (pl-. "href->{x}=" (pl-gethash-deref $href "x")))

;; say ""
(pl-say "")

;; say "--- objects ---"
(pl-say "--- objects ---")

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

;; sub incr { ... }
(defun pl-incr (&rest %_args)
  (let ((@_ (make-array (length %_args) :adjustable t :fill-pointer t :initial-contents %_args)))
    (block nil
      ;; my $self = shift
            (pl-setf $self (pl-shift @_))
      
      ;; $self->{count}++
            (pl-++-post (pl-gethash-deref $self "count"))
      
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

;;; end package Counter

;;; package main

;; my $counter = Counter->new(0)
(pl-setf $counter (pl-method-call "Counter" 'new 0))

;; say "counter=" . $counter->get()
(pl-say (pl-. "counter=" (pl-method-call $counter 'get)))

;; $counter->incr()
(pl-method-call $counter 'incr)

;; $counter->incr()
(pl-method-call $counter 'incr)

;; say "after 2 incr=" . $counter->get()
(pl-say (pl-. "after 2 incr=" (pl-method-call $counter 'get)))

;; say "ref=" . ref($counter)
(pl-say (pl-. "ref=" (let ((*wantarray* t)) (pl-ref $counter))))

;; say ""
(pl-say "")

;; say "=== All Tests Complete ==="
(pl-say "=== All Tests Complete ===")

