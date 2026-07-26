; A let value with a DECLARED (List ...) output but no properness
; certificate gives the bound variable no list knowledge: typed is not
; bound, so the value could arrive as an unbound variable or fail to be a
; list at all, and the nonemptiness narrowing must not fire. first-list
; keeps its semidet verdict at this site and the -[det]-> caller is
; rejected. Compare strictdet_collapse_let_nonempty.metta, where a collapse
; form supplies the guarantee.
(: first-list (-[semidet]-> (List (List Number)) (List Number)))
(= (first-list (cons $head $_tail)) $head)

(: pass-through (-> (List (List Number)) (List (List Number))))
(= (pass-through $xs) $xs)

(: pick-first (-[det]-> (List (List Number)) (List Number)))
(= (pick-first $raw)
   (let $values
      (pass-through $raw)
      (if (== $values ())
         ()
         (first-list $values))))

!(pick-first ((1) (2)))
