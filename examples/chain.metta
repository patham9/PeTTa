; Chain example tests
!(test (chain (+ 2 4) $n (* 3 $n)) 
       18)

!(test (chain (+ 1 3) $n (chain (* 2 $n) $m (+ $n $m))) 
       12)