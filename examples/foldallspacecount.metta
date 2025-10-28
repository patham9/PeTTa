(foo 1)
(foo 2)
(foo 3)

(= (countitem) (let $x (match &self (foo $1) (foo $1)) 1))
(= (merge $a $b) (+ $a $b))
(= (spacecount $x) (foldall merge (countitem) 0))

!(test (foldall merge (countitem) 0) 3)
