(= (f1a)
   (foldl-atom (1 2 3 4) 0 $acc $x (+ $acc $x)))
(= (f2a)
   (map-atom (1 2 3) $x (+ $x 1)))
(= (f3a)
   (filter-atom (1 2 3 4 5) $x (> $x 3)))

(= (foldfun $a $b) (+ $a $b))
(= (mapfun $a) (+ $a 1))
(= (filterfun $x) (> $x 3))

(= (f1b)
   (foldl-atom (1 2 3 4) 0 foldfun))
(= (f2b)
   (map-atom (1 2 3) mapfun))
(= (f3b)
   (filter-atom (1 2 3 4 5) filterfun))

!(test (f1a) 10)
!(test (f2a) (2 3 4))
!(test (f3a) (4 5))

!(test (f1b) 10)
!(test (f2b) (2 3 4))
!(test (f3b) (4 5))


(= (foldfun2 $a $b) (append $a $b))
!(foldl-atom ((1 2) (3 4) (5 6)) () $acc $x (append $acc $x))
