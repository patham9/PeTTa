;First check out constanthead.metta for the simpler case:
(= (len ()) 0)
(= (len (cons $Head $Tail))
   (let $N0 (len $Tail)
        (+ $N0 1)))

!(test (let (cons $Head $Tail) (1 2 3 4 5 6) ($Head $Tail))
            (1 (2 3 4 5 6)))
!(test (len (1 2 3)) 3)
!(test (cons 42 ()) (42))
;Now please out functionhead.metta for a generalization of the idea!
