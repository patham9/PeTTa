(= (infinityfilter)
   (for (($x (range 1 inf)))
        (if (< $x 10)
            (add-atom &self (foo $x))
            (empty))))

(= (finitefilter)
   (for (($x (20 21 22 23 24 25 26 27 28 29)))
        (if (< $x 25)
            (add-atom &self (foo $x))
            (empty))))

!(test ((infinityfilter) (finitefilter) (collapse (match &self (foo $x) $x)))
       (false false (1 2 3 4 5 6 7 8 9 20 21 22 23 24)))
