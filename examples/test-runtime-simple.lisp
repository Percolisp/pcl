;; use v5.32
;; (include handling not yet implemented)

;; use strict
;; (include handling not yet implemented)

;; use warnings
;; (include handling not yet implemented)

;; say "=== Arithmetic ==="
(pl-say "=== Arithmetic ===")

;; my $a = 10
(pl-setf $a 10)

;; my $b = 3
(pl-setf $b 3)

;; say "a=" . $a . ", b=" . $b
(pl-say (pl-. (pl-. (pl-. "a=" $a) ", b=") $b))

;; say "a+b=" . ($a + $b)
(pl-say (pl-. "a+b=" (pl-+ $a $b)))

;; say "a-b=" . ($a - $b)
(pl-say (pl-. "a-b=" (pl-- $a $b)))

;; say "a*b=" . ($a * $b)
(pl-say (pl-. "a*b=" (pl-* $a $b)))

;; say "a%b=" . ($a % $b)
(pl-say (pl-. "a%b=" (pl-% $a $b)))

;; say "a**b=" . ($a ** $b)
(pl-say (pl-. "a**b=" (pl-** $a $b)))

;; say ""
(pl-say "")

;; say "=== Increment ==="
(pl-say "=== Increment ===")

;; my $x = 5
(pl-setf $x 5)

;; say "x=" . $x
(pl-say (pl-. "x=" $x))

;; say "x++=" . $x++
(pl-say (pl-. "x++=" (pl-++-post $x)))

;; say "now x=" . $x
(pl-say (pl-. "now x=" $x))

;; say "++x=" . ++$x
(pl-say (pl-. "++x=" (pl-++-pre $x)))

;; say "now x=" . $x
(pl-say (pl-. "now x=" $x))

;; say ""
(pl-say "")

;; say "=== Compound ==="
(pl-say "=== Compound ===")

;; my $n = 100
(pl-setf $n 100)

;; say "n=" . $n
(pl-say (pl-. "n=" $n))

;; $n += 10
(pl-incf $n 10)

;; say "n+10=" . $n
(pl-say (pl-. "n+10=" $n))

;; $n -= 5
(pl-decf $n 5)

;; say "n-5=" . $n
(pl-say (pl-. "n-5=" $n))

;; $n *= 2
(pl-*= $n 2)

;; say "n*2=" . $n
(pl-say (pl-. "n*2=" $n))

;; say ""
(pl-say "")

;; say "=== Strings ==="
(pl-say "=== Strings ===")

;; my $greeting = "Hello"
(pl-setf $greeting "Hello")

;; my $name = "World"
(pl-setf $name "World")

;; my $msg = $greeting . ", " . $name . "!"
(pl-setf $msg (pl-. (pl-. (pl-. $greeting ", ") $name) "!"))

;; say "msg=" . $msg
(pl-say (pl-. "msg=" $msg))

;; say "len=" . length($msg)
(pl-say (pl-. "len=" (pl-length $msg :wantarray t)))

;; my $str = "test"
(pl-setf $str "test")

;; $str .= "_suffix"
(pl-.= $str "_suffix")

;; say "concat=" . $str
(pl-say (pl-. "concat=" $str))

;; my $repeated = "ab" x 4
(pl-setf $repeated (pl-x "ab" 4))

;; say "repeat=" . $repeated
(pl-say (pl-. "repeat=" $repeated))

;; say ""
(pl-say "")

;; say "=== Compare ==="
(pl-say "=== Compare ===")

;; my $p = 5
(pl-setf $p 5)

;; my $q = 10
(pl-setf $q 10)

;; my $r = 5
(pl-setf $r 5)

;; say "p==r: " . ($p == $r ? "T" : "F")
(pl-say (pl-. "p==r: " (pl-if (pl-== $p $r) "T" "F")))

;; say "p==q: " . ($p == $q ? "T" : "F")
(pl-say (pl-. "p==q: " (pl-if (pl-== $p $q) "T" "F")))

;; say "p<q: " . ($p < $q ? "T" : "F")
(pl-say (pl-. "p<q: " (pl-if (pl-< $p $q) "T" "F")))

;; say "p<=r: " . ($p <= $r ? "T" : "F")
(pl-say (pl-. "p<=r: " (pl-if (pl-<= $p $r) "T" "F")))

;; my $s1 = "apple"
(pl-setf $s1 "apple")

;; my $s2 = "banana"
(pl-setf $s2 "banana")

;; say "lt: " . ($s1 lt $s2 ? "T" : "F")
(pl-say (pl-. "lt: " (pl-if (pl-lt $s1 $s2) "T" "F")))

;; say "eq: " . ($s1 eq $s1 ? "T" : "F")
(pl-say (pl-. "eq: " (pl-if (pl-eq $s1 $s1) "T" "F")))

;; say ""
(pl-say "")

;; say "=== While ==="
(pl-say "=== While ===")

;; my $i = 0
(pl-setf $i 0)

;; my $sum = 0
(pl-setf $sum 0)

;; while ($i < 5) {     $sum += $i;     say "i=" . $i . " sum=" . $sum;     $i++; }
(pl-while (pl-< $i 5)
  ;; $sum += $i
    (pl-incf $sum $i)
  
  ;; say "i=" . $i . " sum=" . $sum
    (pl-say (pl-. (pl-. (pl-. "i=" $i) " sum=") $sum))
  
  ;; $i++
    (pl-++-post $i)
  
)

;; say "final=" . $sum
(pl-say (pl-. "final=" $sum))

;; say ""
(pl-say "")

;; say "=== For ==="
(pl-say "=== For ===")

;; my $product = 1
(pl-setf $product 1)

;; for (my $j = 1; $j <= 5; $j++) {     $product *= $j;     say "j=" . $j . " prod=" . $product; }
(pl-for ((pl-setf $j 1))
        ((pl-<= $j 5))
        ((pl-++-post $j))
  ;; $product *= $j
    (pl-*= $product $j)
  
  ;; say "j=" . $j . " prod=" . $product
    (pl-say (pl-. (pl-. (pl-. "j=" $j) " prod=") $product))
  
)

;; say "5!=" . $product
(pl-say (pl-. "5!=" $product))

;; say ""
(pl-say "")

;; say "=== Cond ==="
(pl-say "=== Cond ===")

;; my $val = 42
(pl-setf $val 42)

;; if ($val > 50) {     say "gt50"; } elsif ($val > 40) {     say "gt40"; } else {     say "le40"; }
;; if ($val > 50)
(pl-if (pl-> $val 50)
  (progn
    ;; say "gt50"
        (pl-say "gt50")
    
  )
  ;; elsif ($val > 40)
  (pl-if   (pl-> $val 40)
    (progn
      ;; say "gt40"
            (pl-say "gt40")
      
    )
    ;; else
    (progn
      ;; say "le40"
            (pl-say "le40")
      
    )
  )
)

;; my $result = $val > 0 ? "pos" : "neg"
(pl-setf $result (pl-if (pl-> $val 0) "pos" "neg"))

;; say "sign=" . $result
(pl-say (pl-. "sign=" $result))

;; say ""
(pl-say "")

;; say "=== Logic ==="
(pl-say "=== Logic ===")

;; my $t = 1
(pl-setf $t 1)

;; my $f = 0
(pl-setf $f 0)

;; say "t&&t=" . ($t && $t)
(pl-say (pl-. "t&&t=" (pl-&& $t $t)))

;; say "t&&f=" . ($t && $f)
(pl-say (pl-. "t&&f=" (pl-&& $t $f)))

;; say "f||t=" . ($f || $t)
(pl-say (pl-. "f||t=" (pl-|| $f $t)))

;; say "f||f=" . ($f || $f)
(pl-say (pl-. "f||f=" (pl-|| $f $f)))

;; my $default = 0
(pl-setf $default 0)

;; my $value = $default || 42
(pl-setf $value (pl-|| $default 42))

;; say "or42=" . $value
(pl-say (pl-. "or42=" $value))

;; say ""
(pl-say "")

;; say "=== Done ==="
(pl-say "=== Done ===")

