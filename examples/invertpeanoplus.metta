;Peano plus:
(= (plus Z $y) $y)
(= (plus (S $x) $y)
   (S (plus $x $y)))

;forward: (2+1=3)
!(test (plus (S (S Z)) (S Z))
       (S (S (S Z))))

;half-inverted: (searching for $A): ($A+1=4) -> $A=3
!(test (let (plus $A (S Z)) (S (S (S (S Z)))) $A)
       (S (S (S Z))))

;half-inverted: (searching for $B): (1+$B=4) -> $B=3
!(test (let (plus (S Z) $B) (S (S (S (S Z)))) $B)
       (S (S (S Z))))

;inverted (generate all input combinations that lead to desired output): $A+$B=4
!(test (collapse (let (plus $A $B) (S (S (S (S Z)))) ;-> ($A,$B) in {(0,4),(1,3),(2,2),(3,1),(4,0)}
                      ($A $B)))
       ((Z (S (S (S (S Z)))))
       ((S Z) (S (S (S Z))))
       ((S (S Z)) (S (S Z)))
       ((S (S (S Z))) (S Z))
       ((S (S (S (S Z)))) Z)))


;inverted (first solution only): $A+$B=4 -> ($A,$B)=(0,4)
!(test (once (let (plus $A $B) (S (S (S (S Z))))
                  ($A $B)))
       (Z (S (S (S (S Z))))))
