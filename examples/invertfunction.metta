(= (f $X $Y)
   (append ($X) $Y))

(= (g $X $Y $Z)
   (append ((#+ $X $Z)) $Y))

;List destructuring:
!(test (let (cons $Head $Tail) (1 2 3 4 5 6) ($Head $Tail))
            (1 (2 3 4 5 6)))

;But instead it works for any relational functions:
!(test (let (f $Head $Tail) (1 2 3 4 5 6) ($Head $Tail))
            (1 (2 3 4 5 6)))

;More complex case:
!(test (let (g $X $Y 35) (42 2 3)
            ($X $Y 40))
       (7 (2 3) 40))
