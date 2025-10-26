; Basic test for updated == equality operator

;1. test for numeric equality with different types
!(test (== 1 1.0) true)

!(test (== 0 0.0) true)
!(test (== 1 1.0) true)
!(test (== 42 42.0) true)

; 2. Test atom equality
!(test (== hello hello) true)
!(test (== hello world) false)
!(test (== A a) false)
!(test (== a a) true)

;3. Test list equality
!(test (== () ()) true)
!(test (== (1 2 3) (1 2 3)) true)
!(test (== (1 2) (1 2 3)) false)
; nested lists
!(test (== ((1 2) (3 4)) ((1 2) (3 4))) true)

; Test mixed numeric types in lists
!(test (== (1 2.0 3) (1.0 2 3.0)) true)

!(test (== (1 2 3) (1 0.5 4)) false)
