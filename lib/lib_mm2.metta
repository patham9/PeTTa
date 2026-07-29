;Add
(: ＋ (-[semidet]-> Expression %Undefined%))
(= (＋ $x)
   (add-atom &mork $x))

;Remove
(: － (-[semidet]-> Expression %Undefined%))
(= (－ $x)
   (remove-atom &mork $x))

;Query
(: ? (-[nondet]-> Expression Expression %Undefined%))
(= (? $x $y)
   (match &mork $x $y))

;Transform
(: ~> (-[nondet]-> Expression Expression %Undefined%))
(= (~> $head $body)
   ((add-atom &mork (exec 0 $head $body))
    (mm2-exec &mork 1)))
