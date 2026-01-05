(= (f1 $f $a) (if (< $a 0)
                  ($f nevercalled 42)
                  (if (== $a 0) 
                      (f2 $f (- $a 1))
                      finish)))
(= (f2 $f $a) (if (< $a 0)
                  ($f nevercalled 42)
                  (f1 $f $a)))

!(test (f1 + 2) finish)

(= (f3 $f $n) (if (== $n 0) finish (f4 $f $n)))
(= (f4 $f $n) (f3 $f (- $n 1)))

!(test (f3 + 1) finish)
