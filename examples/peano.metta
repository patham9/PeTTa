(= (add-atom-no-duplicate $Space $Atom)
   (if (== () (collapse (match $Space $Atom $Atom)))
       (add-atom $Space $Atom)
       (empty)))

; For every existing num($t), add num(S $t)
(= (expand-once)
   (case (match &self (num $t) $t) 
         (($x (add-atom-no-duplicate &self (num (S $x)))))))

; ---------- Peano builders ----------
(= (expandK $n)
   (if (== $n 0)
       done
       (let $temp1 (expand-once)
            (expandK (- $n 1)))))

; ---------- driver ----------
(= (indexing-demo-peano $K)
   (let* (($s (add-atom &self (num Z)))
          ($g (expandK $K)))
         (match &self (num $1) $1)))

; ---- run:
!(indexing-demo-peano 300)
