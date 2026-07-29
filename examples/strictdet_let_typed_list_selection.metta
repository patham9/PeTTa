; A let value with a DECLARED (List ...) output carries selection knowledge
; into the continuation. The producer's ordinary runtime result check is the
; enforcement point for this local value, just as a proper-list boundary
; proviso is the enforcement point when the list is a direct parameter.
(: first-list (-[semidet]-> (List (List Number)) (List Number)))
(= (first-list (cons $head $_tail)) $head)

(: pass-through (-[det]-> (List (List Number)) (List (List Number))))
(= (pass-through $xs) $xs)

(: pick-first (-[det]-> (List (List Number)) (List Number)))
(= (pick-first $raw)
   (let $values
      (pass-through $raw)
      (if (== $values ())
         ()
         (first-list $values))))

!(test (pick-first ((1) (2))) (1))
