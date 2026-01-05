(= (g $x) (+ $x 1))

(= (f $g) ($g 1))
(= (f $g) ($g 2))

!(test (collapse (f g)) (2 3))

!(remove-atom &self (= (f $g) ($g 1)))

;should still call a specialized function even though in this case it is only specialization of (= (f $g) ($g 2)) left.
!(test (f g) 3)

!(add-atom &self (= (f $g) ($g 1)))

!(test (f g) (3 2))
