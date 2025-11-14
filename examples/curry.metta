(= (f $a $b) (+ $a $b))

(= (g $a $b $c) (+ $c (+ $a $b)))

(= (show) (repr (f 1)))

!(test (repr (f 1)) "(partial f (1))")
!(test ((f 1) 2) 3)
!(test (repr (g 1 2)) "(partial g (1 2))")

(= (h $A $B)
   (append ($A) $B))

!(test ((h 42) (1 2 3)) (42 1 2 3))
!(test (repr (h 42)) "(partial h (42))")
!(test (map-atom (1 2 3) (+ 1)) (2 3 4))
