(= (f1)
   (foldl-atom (1 2 3 4) 0 $acc $x (+ $acc $x)))
(= (f2)
   (map-atom (1 2 3) $x (+ $x 1)))
(= (f3)
   (filter-atom (1 2 3 4 5) $x (> $x 3)))


!(test (f1) 10)
!(test (f2) (2 3 4))
!(test (f3) (4 5))
