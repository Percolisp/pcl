;;;; Generated from test-02-control.pl
(in-package :pcl)

(pl-let (($x nil) ($y nil) ($z nil)
         ($flag nil) ($active nil)
         ($a nil) ($b nil) ($max nil) ($min nil) ($val nil) ($size nil)
         ($count nil) ($n nil)
         ($sum nil) ($i nil) ($j nil)
         ($row nil) ($col nil)
         ($p nil) ($q nil))

  (pl-say "=== Test 02: Control Flow ===")
  (pl-say "")

  ;; --- if/elsif/else ---
  (pl-say "--- if/elsif/else ---")
  (pl-setf $x 25)
  (pl-if (pl-> $x 50)
    (progn (pl-say "x>50"))
    (pl-if (pl-> $x 20)
      (progn (pl-say "x>20"))
      (pl-if (pl-> $x 10)
        (progn (pl-say "x>10"))
        (progn (pl-say "x<=10")))))

  (pl-setf $y 5)
  (pl-if (pl-> $y 10)
    (progn (pl-say "y>10"))
    (progn (pl-say "y<=10")))

  (pl-setf $z 100)
  (pl-if (pl-> $z 50)
    (progn (pl-say "z>50"))
    nil)

  (pl-say "")

  ;; --- unless ---
  (pl-say "--- unless ---")
  (pl-setf $flag 0)
  (pl-if (pl-not $flag)
    (progn (pl-say "flag is false"))
    nil)

  (pl-setf $active 1)
  (pl-if (pl-not $active)
    (progn (pl-say "not active"))
    nil)
  (pl-say "after unless")

  (pl-say "")

  ;; --- ternary ---
  (pl-say "--- ternary ---")
  (pl-setf $a 10)
  (pl-setf $b 20)
  (pl-setf $max (pl-if (pl-> $a $b) $a $b))
  (pl-say (pl-. "max=" $max))
  (pl-setf $min (pl-if (pl-< $a $b) $a $b))
  (pl-say (pl-. "min=" $min))

  (pl-setf $val 15)
  (pl-setf $size (pl-if (pl-> $val 20) "large" (pl-if (pl-> $val 10) "medium" "small")))
  (pl-say (pl-. "size=" $size))

  (pl-say "")

  ;; --- while ---
  (pl-say "--- while ---")
  (pl-setf $count 0)
  (pl-while (pl-< $count 3)
    (pl-say (pl-. "count=" $count))
    (pl-++-post $count))
  (pl-say (pl-. "done, count=" $count))

  (pl-say "")

  ;; --- until ---
  (pl-say "--- until ---")
  (pl-setf $n 0)
  (pl-while (pl-not (pl->= $n 3))
    (pl-say (pl-. "n=" $n))
    (pl-++-post $n))
  (pl-say (pl-. "done, n=" $n))

  (pl-say "")

  ;; --- for ---
  (pl-say "--- for ---")
  (pl-setf $sum 0)
  (pl-for ((pl-setf $i 1))
          ((pl-<= $i 5))
          ((pl-++-post $i))
    (pl-incf $sum $i)
    (pl-say (pl-. (pl-. (pl-. "i=" $i) " sum=") $sum)))
  (pl-say (pl-. "total=" $sum))

  (pl-say "countdown:")
  (pl-for ((pl-setf $j 3))
          ((pl->= $j 1))
          ((pl----post $j))
    (pl-say (pl-. "  " $j)))
  (pl-say "  go!")

  (pl-say "")

  ;; --- nested ---
  (pl-say "--- nested ---")
  (pl-for ((pl-setf $row 1))
          ((pl-<= $row 2))
          ((pl-++-post $row))
    (pl-for ((pl-setf $col 1))
            ((pl-<= $col 3))
            ((pl-++-post $col))
      (pl-say (pl-. (pl-. (pl-. "r" $row) "c") $col))))

  (pl-say "")

  ;; --- compound ---
  (pl-say "--- compound ---")
  (pl-setf $p 0)
  (pl-setf $q 10)
  (pl-while (pl-&& (pl-< $p 5) (pl-> $q 5))
    (pl-say (pl-. (pl-. (pl-. "p=" $p) " q=") $q))
    (pl-++-post $p)
    (pl----post $q))

  (pl-say "")
  (pl-say "=== Done ==="))
