(= (g $x) (+ $x 1))

(= (f $g) ($g 1))
(= (f $g) 42)

!(test (collapse (f g)) (2 42))

!(remove-atom &self (= (f $g) 42))

!(test (collapse (f g)) (2))

!(add-atom &self (= (f $g) 42))

!(remove-atom &self (= (f $g) ($g 1)))

!(test (collapse (f g)) (42))

!(remove-atom &self (= (f $g) 42))

!(test (collapse (f g)) ((f g)))
