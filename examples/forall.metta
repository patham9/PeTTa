(= (f) 1)
(= (f) 2)

(= (g 1) 1)
(= (g 2) 2)

(= (P $X) (< $X 2))

;Arg-free generator function plus check function
!(test (forall (f) P)
       false)

;Arg-ful generator function plus check function
!(test (forall (g $x) P)
       false)

;Arg-ful generator lambda plus check function
!(test (let $genlambda (|-> ($x) (g $x))
            (forall ($genlambda $z) P))
       false)

;Arg-ful generator function plus check lambda
!(test (let $checklambda (|-> ($v) (< $v 2))
            (forall (g 2) $checklambda))
       false)

;Arg-ful generator function plus check lambda
!(test (let $checklambda (|-> ($v) (< $v 2))
            (forall (g 1) $checklambda))
       true)

;Arg-ful generator function plus check lambda
!(test (let $checklambda (|-> ($v) (< $v 2))
            (forall (g 2) $checklambda))
       false)

;Arg-ful generator lambda plus check lambda
!(test (let* (($checklambda (|-> ($v) (< $v 2)))
              ($genlambda (|-> ($x) (g $x))))
             (forall ($genlambda $z) $checklambda))
       false)

;lambdas as arg directly:
!(test (forall ((|-> ($x) (g $x)) $z)
                (|-> ($v) (< $v 2)))
       false)

;lambdas as arg directly:
!(test (forall ((|-> ($x) (g $x)) $z)
                (|-> ($v) (< $v 20)))
       true)

;lambdas as arg with lambda wrapped in syntactic construct:
!(test (forall ((|-> ($x) (g $x)) $z)
               (if True (|-> ($v) (< $v 2)) 42))
       false)

;lambdas as arg with lambda wrapped in syntactic construct:
!(test (forall ((|-> ($x) (g $x)) $z)
               (if True (|-> ($v) (< $v 20)) 42))
       true)

;lambdas as arg with lambda wrapped in syntactic construct:
!(test (forall ((|-> ($x) (* 100 (g $x))) $z)
               (if True (|-> ($v) (< $v 20)) 42))
       false)
