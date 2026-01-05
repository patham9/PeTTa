(= (map-flat $f ()) ())
(= (map-flat $f (cons $x $xs)) (cons ($f $x) (map-flat $f $xs)))

(= (range $n)
   (if (== $n 0) ()
       (cons $n (range (- $n 1)))))

!(test (let $temp (map-flat (+ 1) (range 1000000)) (length $temp)) 1000000)


(= (fold-nested $f $init ()) $init)
(= (fold-nested $f $init (cons $x $xs))
      (if (is-expr $x)
        (fold-nested $f (fold-nested $f $init $x) $xs)
        (fold-nested $f ($f $init $x) $xs)))

(= (deep-nest $n)
   (if (== $n 0) ()
       (cons (range 50) (deep-nest (- $n 1)))))

!(test (fold-nested + 0 (deep-nest 20000)) 25500000)


(= (apply-many $f $n $x)
   (if (== $n 0) $x
       (apply-many $f (- $n 1) ($f $x))))

!(test (apply-many (+ 1) 100000 0) 100000)

(= (poly $f $n)
   (if (== $n 0) 0
       (+ ($f $n) (poly $f (- $n 1)))))

!(test (poly (+ 1) 1000000) 500001500000)
