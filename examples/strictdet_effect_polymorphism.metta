(: effect-map
   (-[$e]-> (-[$e]-> $a $b) (List $a) (List $b)))
(= (effect-map $f ()) ())
(= (effect-map $f (cons $x $xs))
   (cons ($f $x) (effect-map $f $xs)))

(: effect-inc (-[det]-> Number Number))
(= (effect-inc $x) (+ $x 1))

(: effect-choice (-[nondet]-> Number Number))
(= (effect-choice $x) (superpose ($x (+ $x 10))))

(: effect-positive (-[semidet]-> Number Number))
(= (effect-positive $x) (if (> $x 0) $x))

(: effect-map-det (-[det]-> (List Number) (List Number)))
(= (effect-map-det $xs) (effect-map effect-inc $xs))

(: effect-map-semidet (-[semidet]-> (List Number) (List Number)))
(= (effect-map-semidet $xs) (effect-map effect-positive $xs))

(: effect-map-nondet (-[nondet]-> (List Number) (List Number)))
(= (effect-map-nondet $xs) (effect-map effect-choice $xs))

!(test (effect-map-det (1 2)) (2 3))
!(test (collapse (effect-map-semidet (1 -1))) ())
!(test
   (collapse (effect-map-nondet (1 2)))
   ((1 2) (1 12) (11 2) (11 12)))
