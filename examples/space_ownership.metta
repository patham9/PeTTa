; Store the same equation under two independent owners.
!(bind! &foo (new-space))
!(add-atom &self (= (f) 1))
!(add-atom &foo (= (f) 1))

; Default evaluation uses &self; eval/2 selects &foo explicitly.
!(test (collapse (f)) (1))
!(test (eval (f) &foo) 1)

; Removing &foo's copy leaves &self's data and executable behavior intact.
!(remove-atom &foo (= (f) 1))
!(test (match &self (= (f) $result) $result) 1)
!(test (collapse (f)) (1))
