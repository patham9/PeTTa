; test for numeric equality with different types
!(test (== 1 1.0) true)
; Basic test for updated == equality operator
; Test the core functionality that the project manager requested

; Test numeric equality (int and float with equal magnitude) - the main requirement
!(test (== 0 0.0) true)
!(test (== 1 1.0) true)
!(test (== 42 42.0) true)

; Test atom equality
!(test (== hello hello) true)
!(test (== hello world) false)

; Test list equality
!(test (== () ()) true)
!(test (== (1 2 3) (1 2 3)) true)
!(test (== (1 2) (1 2 3)) false)
!(test (== ((1 2) (3 4)) ((1 2) (3 4))) true)

; Test mixed numeric types in lists
!(test (== (1 2.0 3) (1.0 2 3.0)) true)

!(test (== (1 2 3) (1 0.5 4)) false)