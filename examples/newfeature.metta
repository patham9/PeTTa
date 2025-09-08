(= (f $L $a $b)
   (let $result (+ $a $b)
        (append ($result) $L)))

!(test (match &self (= (f (1 2 3) 40.7 2) ($x 1 2 3)) $x) 42.7)
