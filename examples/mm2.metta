;Add item to space without duplicates (cheap exists check due to 'once' despite 'collapse')
!(add-atom &dup (haha))
(= (add-atom-no-duplicate $Space $Atom)
   (let $st (s (repra $Atom))
        (if (== () (collapse (once (match &dup $st True))))
            ((add-atom &dup $st)
             (add-atom $Space $Atom))
            True)))

;Execute sinks:
(= (MM2_ApplyRuleBody ()) True)
(= (MM2_ApplyRuleBody (cons $headop $rest))
   (and (case $headop
              (((＋ $inside) (add-atom-no-duplicate &self $inside))
               ((－ $inside) (remove-atom &self $inside))
               (($Else)     (add-atom-no-duplicate &self $inside))))
        (MM2_ApplyRuleBody $rest)))

;MM2 space transform based on (Head Body) rewrite rule expression
(= (MM2 ($source $sink))
   (collapse (let (cons $car $cdr) (match &self $source $sink)
                                   (MM2_ApplyRuleBody $cdr))))

;Generator: (separate non-det return of each item)
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

;Spacecount: item count in space without list building
(= (countitem) (let $x (get-atoms &self) 1))
(= (merge $a $b) (+ $a $b))
(= (spacecount) (foldall merge (countitem) 0))
