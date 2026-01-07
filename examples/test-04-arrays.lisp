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

;; say "=== Test 04: Arrays ==="
(pl-say "=== Test 04: Arrays ===")

;; say ""
(pl-say "")

;; say "--- access ---"
(pl-say "--- access ---")

;; my @nums (bare declaration)

;; push @nums, 10
(pl-push @nums 10)

;; push @nums, 20
(pl-push @nums 20)

;; push @nums, 30
(pl-push @nums 30)

;; push @nums, 40
(pl-push @nums 40)

;; push @nums, 50
(pl-push @nums 50)

;; say "nums[0]=" . $nums[0]
(pl-say (pl-. "nums[0]=" (pl-aref @nums 0)))

;; say "nums[2]=" . $nums[2]
(pl-say (pl-. "nums[2]=" (pl-aref @nums 2)))

;; say "nums[4]=" . $nums[4]
(pl-say (pl-. "nums[4]=" (pl-aref @nums 4)))

;; say "nums[-1]=" . $nums[-1]
(pl-say (pl-. "nums[-1]=" (pl-aref @nums -1)))

;; say "nums[-2]=" . $nums[-2]
(pl-say (pl-. "nums[-2]=" (pl-aref @nums -2)))

;; say ""
(pl-say "")

;; say "--- modify ---"
(pl-say "--- modify ---")

;; $nums[1] = 99
(pl-setf (pl-aref @nums 1) 99)

;; say "after [1]=99: " . $nums[1]
(pl-say (pl-. "after [1]=99: " (pl-aref @nums 1)))

;; $nums[0] = $nums[0] + 5
(pl-setf (pl-aref @nums 0) (pl-+ (pl-aref @nums 0) 5))

;; say "after [0]+=5: " . $nums[0]
(pl-say (pl-. "after [0]+=5: " (pl-aref @nums 0)))

;; say ""
(pl-say "")

;; say "--- push/pop ---"
(pl-say "--- push/pop ---")

;; my @stack (bare declaration)

;; push @stack, 1
(pl-push @stack 1)

;; push @stack, 2
(pl-push @stack 2)

;; push @stack, 3
(pl-push @stack 3)

;; say "pushed 1,2,3"
(pl-say "pushed 1,2,3")

;; my $top = pop @stack
(pl-setf $top (pl-pop @stack))

;; say "pop=" . $top
(pl-say (pl-. "pop=" $top))

;; $top = pop @stack
(pl-setf $top (pl-pop @stack))

;; say "pop=" . $top
(pl-say (pl-. "pop=" $top))

;; push @stack, 10
(pl-push @stack 10)

;; say "pushed 10"
(pl-say "pushed 10")

;; $top = pop @stack
(pl-setf $top (pl-pop @stack))

;; say "pop=" . $top
(pl-say (pl-. "pop=" $top))

;; $top = pop @stack
(pl-setf $top (pl-pop @stack))

;; say "pop=" . $top
(pl-say (pl-. "pop=" $top))

;; say ""
(pl-say "")

;; say "--- shift/unshift ---"
(pl-say "--- shift/unshift ---")

;; my @queue (bare declaration)

;; push @queue, 1
(pl-push @queue 1)

;; push @queue, 2
(pl-push @queue 2)

;; push @queue, 3
(pl-push @queue 3)

;; my $first = shift @queue
(pl-setf $first (pl-shift @queue))

;; say "shift=" . $first
(pl-say (pl-. "shift=" $first))

;; $first = shift @queue
(pl-setf $first (pl-shift @queue))

;; say "shift=" . $first
(pl-say (pl-. "shift=" $first))

;; unshift @queue, 10
(pl-unshift @queue 10)

;; say "unshift 10"
(pl-say "unshift 10")

;; $first = shift @queue
(pl-setf $first (pl-shift @queue))

;; say "shift=" . $first
(pl-say (pl-. "shift=" $first))

;; say ""
(pl-say "")

;; say "--- length ---"
(pl-say "--- length ---")

;; my @items (bare declaration)

;; push @items, "a"
(pl-push @items "a")

;; push @items, "b"
(pl-push @items "b")

;; push @items, "c"
(pl-push @items "c")

;; push @items, "d"
(pl-push @items "d")

;; my $len = scalar(@items)
(pl-setf $len (pl-scalar @items))

;; say "length=" . $len
(pl-say (pl-. "length=" $len))

;; my $last_idx = $#items
(pl-setf $last_idx (pl-array-last-index @items))

;; say "last_idx=" . $last_idx
(pl-say (pl-. "last_idx=" $last_idx))

;; say ""
(pl-say "")

;; say "--- foreach ---"
(pl-say "--- foreach ---")

;; my @vals (bare declaration)

;; push @vals, 10
(pl-push @vals 10)

;; push @vals, 20
(pl-push @vals 20)

;; push @vals, 30
(pl-push @vals 30)

;; my $total = 0
(pl-setf $total 0)

;; foreach my $v (@vals) {     say "v=" . $v;     $total += $v; }
(pl-foreach ($v @vals)
  ;; say "v=" . $v
    (pl-say (pl-. "v=" $v))
  
  ;; $total += $v
    (pl-incf $total $v)
  
)

;; say "total=" . $total
(pl-say (pl-. "total=" $total))

;; say ""
(pl-say "")

;; say "--- expressions ---"
(pl-say "--- expressions ---")

;; my @data (bare declaration)

;; push @data, 5
(pl-push @data 5)

;; push @data, 10
(pl-push @data 10)

;; push @data, 15
(pl-push @data 15)

;; my $sum = $data[0] + $data[1] + $data[2]
(pl-setf $sum (pl-+ (pl-+ (pl-aref @data 0) (pl-aref @data 1)) (pl-aref @data 2)))

;; say "sum=" . $sum
(pl-say (pl-. "sum=" $sum))

;; my $prod = $data[0] * $data[2]
(pl-setf $prod (pl-* (pl-aref @data 0) (pl-aref @data 2)))

;; say "prod=" . $prod
(pl-say (pl-. "prod=" $prod))

;; say ""
(pl-say "")

;; say "=== Done ==="
(pl-say "=== Done ===")

