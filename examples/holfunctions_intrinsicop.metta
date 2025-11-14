(= (mymap $f ()) ())
(= (mymap $f (cons $x $xs)) (cons ($f $x) (mymap $f $xs)))

(= (eq $a $b) (== $a $b))

!(test (mymap (== 1) (1 2 3)) (mymap (eq 1) (1 2 3)))
