(= (f $L $a $b)
   (let $result (+ $a $b)
        (append ($result) $L)))

(= (match &self $1 $2) $2)

!(test (match &self (= (f (1 2 3) $x 2) (42.7 1 2 3)) $x) 40.7)
