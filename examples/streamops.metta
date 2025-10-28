!(test (collapse (unique (superpose (a b c d d)))) (a b c d))

!(test (collapse (union (superpose (a b b c)) (superpose (b c c d)))) (a b b c b c c d))

!(test (collapse (intersection (superpose (a b c c)) (superpose (b c c c d)))) (b c c))

!(test (collapse (subtraction (superpose (a b b c)) (superpose (b c c d)))) (a b))
