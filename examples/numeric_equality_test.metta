; Test numeric equality: 1 should equal 1.0
!(test (== 1 1.0) True)

; Test that regular equality still works for same types
!(test (== 1 1) True)
!(test (== 1.0 1.0) True)
!(test (== 2 2.0) True)
!(test (== 1.5 1.5) True)

; Test non-equality
!(test (== 1 2) False)
!(test (== 1.0 2.0) False)
!(test (== 1 2.0) False)

