(= (f $L $a $b)
   (let $result (+ $a $b)
        (append ($result) $L)))

!(test (match &self (= (f (1 2 3) $x 2) (42.7 1 2 3)) $x) 40.7)
