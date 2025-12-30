!(import! &self (library lib_patrick))

(= (mirror (@ $L (cons $head $tail)))
   (append (reverse $L) $tail))

!(test (mirror (h a n n e s)) (s e n n a h a n n e s))

!(test (collapse (for $x (1 2 3 4 5 6)
                      (if (> $x 3) $x)))
       (4 5 6))
      
!(test (iterate 0 10
                1 (|-> ($i $x)
                       (+ $x $i)))
       46)
