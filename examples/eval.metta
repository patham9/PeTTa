(= (f $L $a $b)
   (let $result (+ $a $b)
        (append ($result) $L)))

;Match does not automatically interpret the returned pattern as code:
;!(match &self (= (f 40.7 2) $x) $x)

;But we can eval after it if we desire (invokes translator.pl and calls the translated goals):
!(test (let $fbody_specialized (match &self (= (f (42) 40.7 2) $x) $x) (eval $fbody_specialized))
       (42.7 42))

;Additionally we can emulate eval by using add-atom and remove-atom (not recommended):
(= (evalCustom $body)
   (let* (($a   (add-atom &self (= (myfunc) $body)))
          ($res (reduce (myfunc)))
          ($r   (remove-atom &self (= (myfunc) $body))))
         $res))

!(test (evalCustom (match &self (= (f (42) 40.7 2) $x) $x))
       (42.7 42))
