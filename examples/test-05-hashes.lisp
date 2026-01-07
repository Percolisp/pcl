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

;; say "=== Test 05: Hashes ==="
(pl-say "=== Test 05: Hashes ===")

;; say ""
(pl-say "")

;; say "--- access ---"
(pl-say "--- access ---")

;; my %person (bare declaration)

;; $person{name} = "Alice"
(pl-setf (pl-gethash %person "name") "Alice")

;; $person{age} = 30
(pl-setf (pl-gethash %person "age") 30)

;; $person{city} = "Paris"
(pl-setf (pl-gethash %person "city") "Paris")

;; say "name=" . $person{name}
(pl-say (pl-. "name=" (pl-gethash %person "name")))

;; say "age=" . $person{age}
(pl-say (pl-. "age=" (pl-gethash %person "age")))

;; say "city=" . $person{city}
(pl-say (pl-. "city=" (pl-gethash %person "city")))

;; say ""
(pl-say "")

;; say "--- modify ---"
(pl-say "--- modify ---")

;; $person{age} = 31
(pl-setf (pl-gethash %person "age") 31)

;; say "age after birthday: " . $person{age}
(pl-say (pl-. "age after birthday: " (pl-gethash %person "age")))

;; $person{country} = "France"
(pl-setf (pl-gethash %person "country") "France")

;; say "added country: " . $person{country}
(pl-say (pl-. "added country: " (pl-gethash %person "country")))

;; say ""
(pl-say "")

;; say "--- exists/delete ---"
(pl-say "--- exists/delete ---")

;; say "skipped"
(pl-say "skipped")

;; say ""
(pl-say "")

;; say "--- keys/values ---"
(pl-say "--- keys/values ---")

;; my %scores (bare declaration)

;; $scores{alice} = 95
(pl-setf (pl-gethash %scores "alice") 95)

;; $scores{bob} = 87
(pl-setf (pl-gethash %scores "bob") 87)

;; $scores{charlie} = 92
(pl-setf (pl-gethash %scores "charlie") 92)

;; my @k = keys %scores
(pl-setf @k (let ((*wantarray* t)) (pl-keys %scores)))

;; say "num keys=" . scalar(@k)
(pl-say (pl-. "num keys=" (let ((*wantarray* t)) (pl-scalar @k))))

;; my @v = values %scores
(pl-setf @v (let ((*wantarray* t)) (pl-values %scores)))

;; my $sum = 0
(pl-setf $sum 0)

;; foreach my $val (@v) {     $sum += $val; }
(pl-foreach ($val @v)
  ;; $sum += $val
    (pl-incf $sum $val)
  
)

;; say "sum values=" . $sum
(pl-say (pl-. "sum values=" $sum))

;; say ""
(pl-say "")

;; say "--- numeric ---"
(pl-say "--- numeric ---")

;; my %counts (bare declaration)

;; $counts{a} = 0
(pl-setf (pl-gethash %counts "a") 0)

;; $counts{b} = 0
(pl-setf (pl-gethash %counts "b") 0)

;; $counts{c} = 0
(pl-setf (pl-gethash %counts "c") 0)

;; $counts{a}++
(pl-++-post (pl-gethash %counts "a"))

;; $counts{a}++
(pl-++-post (pl-gethash %counts "a"))

;; $counts{b}++
(pl-++-post (pl-gethash %counts "b"))

;; say "a=" . $counts{a}
(pl-say (pl-. "a=" (pl-gethash %counts "a")))

;; say "b=" . $counts{b}
(pl-say (pl-. "b=" (pl-gethash %counts "b")))

;; say "c=" . $counts{c}
(pl-say (pl-. "c=" (pl-gethash %counts "c")))

;; $counts{a} += 10
(pl-incf (pl-gethash %counts "a") 10)

;; say "a after +=10: " . $counts{a}
(pl-say (pl-. "a after +=10: " (pl-gethash %counts "a")))

;; say ""
(pl-say "")

;; say "--- expressions ---"
(pl-say "--- expressions ---")

;; my %data (bare declaration)

;; $data{x} = 10
(pl-setf (pl-gethash %data "x") 10)

;; $data{y} = 20
(pl-setf (pl-gethash %data "y") 20)

;; $data{z} = 30
(pl-setf (pl-gethash %data "z") 30)

;; my $total = $data{x} + $data{y} + $data{z}
(pl-setf $total (pl-+ (pl-+ (pl-gethash %data "x") (pl-gethash %data "y")) (pl-gethash %data "z")))

;; say "total=" . $total
(pl-say (pl-. "total=" $total))

;; my $cond = $data{x} < $data{y} ? "x<y" : "x>=y"
(pl-setf $cond (pl-if (pl-< (pl-gethash %data "x") (pl-gethash %data "y")) "x<y" "x>=y"))

;; say "compare: " . $cond
(pl-say (pl-. "compare: " $cond))

;; say ""
(pl-say "")

;; say "=== Done ==="
(pl-say "=== Done ===")

