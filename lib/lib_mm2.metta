;Add
(: ＋ (-> Expression %Undefined%))
(= (＋ $x)
   (add-atom &mork $x))

;Remove
(: － (-> Expression %Undefined%))
(= (－ $x)
   (remove-atom &mork $x))

;Query
(: ? (-> Expression Expression %Undefined%))
(= (? $x $y)
   (match &mork $x $y))

;Transform
(: ~> (-> Expression Expression %Undefined%))
(= (~> $head $body)
   ((add-atom &mork (exec 0 $head $body))
    (mm2-exec &mork 1)))
