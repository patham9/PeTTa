(= (match-types $A $B $Then $Else) (if (== $A $B) $Then $Else))

!(match-types Atom Atom "Matched!" "Didn't match")

!(match-types Atom Number "Matched!" "Didn't match")

(= (match-type-or $value $type1 $type2) (match-types $type1 $type2 True $value))

!(test (match-type-or True Number Number) True)
!(test (match-type-or False Number Number) True)
!(test (match-type-or True Number Bool) True)
!(test (match-type-or False Number Bool) False)
