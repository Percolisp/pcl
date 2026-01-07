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

;; say "=== Test 06: References ==="
(pl-say "=== Test 06: References ===")

;; say ""
(pl-say "")

;; say "--- scalar refs ---"
(pl-say "--- scalar refs ---")

;; my $x = 42
(pl-setf $x 42)

;; say "x=" . $x
(pl-say (pl-. "x=" $x))

;; my $ref = \$x
(pl-setf $ref (pl-backslash $x))

;; say "ref points to x"
(pl-say "ref points to x")

;; my $val = $$ref
(pl-setf $val (pl-$ $ref))

;; say "deref=" . $val
(pl-say (pl-. "deref=" $val))

;; $$ref = 100
(pl-setf (pl-$ $ref) 100)

;; say "after $$ref=100, x=" . $x
(pl-say (pl-. (pl-string_concat "after " "$") $x))

;; $$ref += 5
(pl-incf (pl-$ $ref) 5)

;; say "after $$ref+=5, x=" . $x
(pl-say (pl-. (pl-string_concat "after " "$") $x))

;; say ""
(pl-say "")

;; say "--- pass by ref ---"
(pl-say "--- pass by ref ---")

;; sub increment_ref { ... }
(defun pl-increment_ref (&rest @_)
  (block nil
    ;; my $r = shift
        (pl-setf $r (pl-shift @_))
    
    ;; $$r = $$r + 1
        (pl-setf (pl-$ $r) (pl-+ (pl-$ $r) 1))
    
  )
)

;; my $counter = 0
(pl-setf $counter 0)

;; say "counter=" . $counter
(pl-say (pl-. "counter=" $counter))

;; increment_ref(\$counter)
(pl-increment_ref (pl-backslash $counter))

;; say "after increment: " . $counter
(pl-say (pl-. "after increment: " $counter))

;; increment_ref(\$counter)
(pl-increment_ref (pl-backslash $counter))

;; say "after increment: " . $counter
(pl-say (pl-. "after increment: " $counter))

;; sub double_ref { ... }
(defun pl-double_ref (&rest @_)
  (block nil
    ;; my $r = shift
        (pl-setf $r (pl-shift @_))
    
    ;; $$r = $$r * 2
        (pl-setf (pl-$ $r) (pl-* (pl-$ $r) 2))
    
  )
)

;; my $num = 5
(pl-setf $num 5)

;; double_ref(\$num)
(pl-double_ref (pl-backslash $num))

;; say "5 doubled=" . $num
(pl-say (pl-. "5 doubled=" $num))

;; say ""
(pl-say "")

;; say "--- array refs ---"
(pl-say "--- array refs ---")

;; my @arr (bare declaration)

;; push @arr, 10
(pl-push @arr 10)

;; push @arr, 20
(pl-push @arr 20)

;; push @arr, 30
(pl-push @arr 30)

;; my $aref = \@arr
(pl-setf $aref (pl-backslash @arr))

;; say "aref->[0]=" . $aref->[0]
(pl-say (pl-. "aref->[0]=" (pl-aref-deref $aref 0)))

;; say "aref->[2]=" . $aref->[2]
(pl-say (pl-. "aref->[2]=" (pl-aref-deref $aref 2)))

;; $aref->[1] = 99
(pl-setf (pl-aref-deref $aref 1) 99)

;; say "after aref->[1]=99, arr[1]=" . $arr[1]
(pl-say (pl-. "after aref->[1]=99, arr[1]=" (pl-aref @arr 1)))

;; say ""
(pl-say "")

;; say "--- hash refs ---"
(pl-say "--- hash refs ---")

;; my %hash (bare declaration)

;; $hash{name} = "Bob"
(pl-setf (pl-gethash %hash "name") "Bob")

;; $hash{age} = 25
(pl-setf (pl-gethash %hash "age") 25)

;; my $href = \%hash
(pl-setf $href (pl-backslash %hash))

;; say "href->{name}=" . $href->{name}
(pl-say (pl-. "href->{name}=" (pl-gethash-deref $href "name")))

;; say "href->{age}=" . $href->{age}
(pl-say (pl-. "href->{age}=" (pl-gethash-deref $href "age")))

;; $href->{age} = 26
(pl-setf (pl-gethash-deref $href "age") 26)

;; say "after href->{age}=26, hash{age}=" . $hash{age}
(pl-say (pl-. "after href->{age}=26, hash{age}=" (pl-gethash %hash "age")))

;; $href->{city} = "NYC"
(pl-setf (pl-gethash-deref $href "city") "NYC")

;; say "added via ref: " . $hash{city}
(pl-say (pl-. "added via ref: " (pl-gethash %hash "city")))

;; say ""
(pl-say "")

;; say "--- anon array ---"
(pl-say "--- anon array ---")

;; my $anon_arr = [1, 2, 3, 4, 5]
(pl-setf $anon_arr (list 1 2 3 4 5))

;; say "anon[0]=" . $anon_arr->[0]
(pl-say (pl-. "anon[0]=" (pl-aref-deref $anon_arr 0)))

;; say "anon[4]=" . $anon_arr->[4]
(pl-say (pl-. "anon[4]=" (pl-aref-deref $anon_arr 4)))

;; $anon_arr->[2] = 30
(pl-setf (pl-aref-deref $anon_arr 2) 30)

;; say "after [2]=30: " . $anon_arr->[2]
(pl-say (pl-. "after [2]=30: " (pl-aref-deref $anon_arr 2)))

;; say ""
(pl-say "")

;; say "--- anon hash ---"
(pl-say "--- anon hash ---")

;; my $anon_hash = {x => 10, y => 20, z => 30}
(pl-setf $anon_hash (pl-hash "x" 10 "y" 20 "z" 30))

;; say "x=" . $anon_hash->{x}
(pl-say (pl-. "x=" (pl-gethash-deref $anon_hash "x")))

;; say "y=" . $anon_hash->{y}
(pl-say (pl-. "y=" (pl-gethash-deref $anon_hash "y")))

;; $anon_hash->{z} = 300
(pl-setf (pl-gethash-deref $anon_hash "z") 300)

;; say "after z=300: " . $anon_hash->{z}
(pl-say (pl-. "after z=300: " (pl-gethash-deref $anon_hash "z")))

;; say ""
(pl-say "")

;; say "--- nested ---"
(pl-say "--- nested ---")

;; my $data = {     name => "Test",     values => [100, 200, 300] }
(pl-setf $data (pl-hash "name" "Test" "values" (list 100 200 300)))

;; say "name=" . $data->{name}
(pl-say (pl-. "name=" (pl-gethash-deref $data "name")))

;; say "values[0]=" . $data->{values}->[0]
(pl-say (pl-. "values[0]=" (pl-aref (pl-gethash-deref $data "values") 0)))

;; say "values[2]=" . $data->{values}->[2]
(pl-say (pl-. "values[2]=" (pl-aref (pl-gethash-deref $data "values") 2)))

;; $data->{values}->[1] = 999
(pl-setf (pl-aref (pl-gethash-deref $data "values") 1) 999)

;; say "after [1]=999: " . $data->{values}->[1]
(pl-say (pl-. "after [1]=999: " (pl-aref (pl-gethash-deref $data "values") 1)))

;; say ""
(pl-say "")

;; say "--- ref() ---"
(pl-say "--- ref() ---")

;; my $scalar = 42
(pl-setf $scalar 42)

;; my $s_ref = \$scalar
(pl-setf $s_ref (pl-backslash $scalar))

;; my $a_ref = [1, 2, 3]
(pl-setf $a_ref (list 1 2 3))

;; my $h_ref = {a => 1}
(pl-setf $h_ref (pl-hash "a" 1))

;; say "ref(scalar)=" . (ref($scalar) || "not a ref")
(pl-say (pl-. "ref(scalar)=" (pl-|| (let ((*wantarray* t)) (pl-ref $scalar)) "not a ref")))

;; say "ref(s_ref)=" . ref($s_ref)
(pl-say (pl-. "ref(s_ref)=" (let ((*wantarray* t)) (pl-ref $s_ref))))

;; say "ref(a_ref)=" . ref($a_ref)
(pl-say (pl-. "ref(a_ref)=" (let ((*wantarray* t)) (pl-ref $a_ref))))

;; say "ref(h_ref)=" . ref($h_ref)
(pl-say (pl-. "ref(h_ref)=" (let ((*wantarray* t)) (pl-ref $h_ref))))

;; say ""
(pl-say "")

;; say "=== Done ==="
(pl-say "=== Done ===")

