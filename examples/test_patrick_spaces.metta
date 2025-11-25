; Test Patrick's Static Space Approach (NO bind! needed!)

!(println! "=== Patrick's Static Spaces - No bind! Required ===")

; Approach 1: Just use space names directly!
; Spaces are auto-created on first add-atom

!(add-atom &kb1 (parent alice bob))
!(add-atom &kb1 (parent bob charlie))

!(test (collapse (match &kb1 (parent alice $x) $x))
       (bob))

; Approach 2: Multiple independent spaces
!(add-atom &facts (fact "sky" "blue"))
!(add-atom &facts (fact "grass" "green"))

!(add-atom &rules (rule "if-sunny" "go-outside"))
!(add-atom &rules (rule "if-rainy" "stay-inside"))

!(test (collapse (match &facts (fact $x $y) ($x $y)))
       (("sky" "blue") ("grass" "green")))

!(test (collapse (match &rules (rule $x $y) ($x $y)))
       (("if-sunny" "go-outside") ("if-rainy" "stay-inside")))

; Approach 3: Spaces are completely isolated
!(test (collapse (match &facts (rule $x $y) found))
       ())  ; ← No rules in &facts

!(test (collapse (match &rules (fact $x $y) found))
       ())  ; ← No facts in &rules

; Approach 4: Can use any &name
!(add-atom &database (user alice (email "alice@example.com")))
!(add-atom &cache (cached "query1" result))
!(add-atom &temp_work (processing task1))

!(test (collapse (match &database (user alice $x) $x))
       ((email "alice@example.com")))

!(println! "=== Key Insight ===")
!(println! "Patrick's approach: Just use &spacename directly")
!(println! "No need for: !(bind! &kb (new-space))")
!(println! "Just do: !(add-atom &kb ...)")
!(println! "Spaces auto-created on first use!")

!(println! "=== All tests passed! ===")
