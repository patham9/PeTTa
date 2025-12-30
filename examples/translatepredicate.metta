!(test (progn (translatePredicate (is $x 2))
              (translatePredicate (+ $x 40 $z)) $z)
       42)
