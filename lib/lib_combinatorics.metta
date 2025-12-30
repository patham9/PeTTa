;Range number generator, separate non-det return of each item:
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

;Combine two elements of $L uniquely e.g. (a b) but not (b a):
(= (choose2 $L)
   (let $j (range 0 (length $L))
           (let $i (range 0 $j)
                ((index-atom $L $i) (index-atom $L $j)))))
(= (choose2l $L)
   (collapse (choose2 $L)))

; choose exactly k elements, unordered
(= (chooseKl $L 0) (let $temp (cut) (())))
(= (chooseKl () $k) (let $temp (cut) ()))
(= (chooseKl (cons $h $t) $k)
   (append (map-atom (chooseKl $t (- $k 1)) (|-> ($c) (cons $h $c)))
           (chooseKl $t $k)))
(= (chooseK $L $k)
   (let $options (chooseKl $L $k) (superpose $options)))


;Take the first K items from the list:
(= (takeK $k ()) ())
(= (takeK $k (cons $head $tail))
   (if (> $k 0)
       (cons $head (takeK (- $k 1) $tail))
       ()))
