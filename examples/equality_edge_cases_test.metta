; Test that non-numeric equality still works correctly
!(test (== "hello" "hello") True)
!(test (== "hello" "world") False)
!(test (== a a) True)
!(test (== a b) False)
!(test (== (1 2 3) (1 2 3)) True)
!(test (== (1 2) (1 2 3)) False)

; Test numeric edge cases
!(test (== 0 0.0) True)
!(test (== -1 -1.0) True)
!(test (== 1.0 1) True)
!(test (== 3.14159 3.14159) True)

