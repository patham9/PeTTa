(= (if-nondet $y)
  (if (superpose $y) a b))
  
(= (case-nondet $y)
   (case (superpose $y)
         ((True a)
          (False b))))

!(test (collapse (if-nondet (True False True))) (a b a))

!(test (collapse (case-nondet (True False True))) (a b a))
