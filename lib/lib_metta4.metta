;generate a list full of unique variables:
(= (varrange $K $N)
   (if (< $K $N)
       (superpose ($X (varrange (+ $K 1) $N)))
       (empty)))

;Add progn metta4 functions for procedural execution:
(= (addProgn $K)
   (if (> $K 0)
       (let* (($varlist (collapse (varrange 0 $K)))
              ($firstvar (car-atom $varlist))
              ($lastvar (car-atom (reverse $varlist)))
              ($Headern (append (quote (progn)) $varlist))
              ($Header1 (append (quote (prog1)) $varlist))
              ($temp1 (add-atom &self (= $Header1 $firstvar)))
              ($temp2 (add-atom &self (= $Headern $lastvar))))
             (addProgn (- $K 1)))
       (empty)))
!(addProgn 50)
