(= (myf $M)
   (and (and (member a $M)
             (member b $M))
        (== (size-atom $M) 3)))

!(test (let $constraint (once (myf $M)) $M)
       (a b))
