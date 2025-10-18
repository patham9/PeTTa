!(test (unique (1 1 $a $a)) (1 $a))

(= (dup $a) ($a $a))

!(test (map-flat (1 (2 3)) dup) ((1 1) ((2 3) (2 3))))

!(test (map-nested (1 (2 3)) dup) ((1 1) ((2 2) (3 3))))

(= (add $a $b) ($a $b))

!(test (fold-flat (1 (2 3)) a add) ((a 1) (2 3)))

!(test (fold-nested (1 (2 3)) a add) (((a 1) 2) 3))
