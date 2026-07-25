; The determinism of a builtin is whatever builtin_call_determinism/3 records
; and nothing else, so the table has to carry every builtin that genuinely is
; deterministic or legitimate proofs disappear. This pins the ones a det body
; is expected to be able to use: arithmetic, comparison, booleans, reflection
; and the total list operations.
(: arith (-[det]-> Number Number Number))
(= (arith $a $b) (+ (* $a 2) (min $b (max $a 3))))

(: compare-them (-[det]-> Number Number Bool))
(= (compare-them $a $b) (and (< $a $b) (not (== $a $b))))

(: shape (-[det]-> Expression Number))
(= (shape $x) (size-atom (cdr-atom $x)))

(: extend (-[det]-> (List Number) Number (List Number)))
(= (extend $xs $x) (union-atom $xs ($x)))

(: describe (-[det]-> Expression String))
(= (describe $x) (repr (get-metatype $x)))

!(test (arith 4 5) 12)
!(test (compare-them 1 2) true)
!(test (shape (a b c)) 2)
!(test (extend (1 2) 3) (1 2 3))
!(test (describe (a b)) "Expression")
