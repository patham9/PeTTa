; %%%% Hyperon-Experimental compatibility library. %%%%
; Many of the included functions were considered ad hoc and
; straightforward to express directly in MeTTa itself:

;Equality and Reduction
(= (if-equal $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(= (if-equal2 $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(= (assertEqual $A $B)
   (assert (== $A $B)))

(= (assertAlphaEqual $A $B)
   (assert (=alpha $A $B)))

;Error Handling
(= (if-error $X $A $B)
   (if (== (car-atom $X) Error) 
       $A $B))

(= (return-on-error $result $B)
   (if-error $result
             $result
             $B))

(= (match-types $A $B $Then $Else)
   (if (== $A $B) $Then $Else))

(= (match-type-or $value $type1 $type2)
   (match-types $type1 $type2 True $value))

(: add-reduct (-> %Undefined% Expression Bool))
(= (add-reduct $space $f)
   (let* (($headbody (cdr-atom $f))
          ($head (car-atom $headbody))
          ($body (cdr-atom $headbody))
          ($bodyreduced (eval $body)))
        (add-atom $space (= $head $bodyreduced))))

;Evaluation control
(= (for-each-in-atom $L $f)
   (map-flat $L $f))

(: evalc (-> Expression %Undefined% %Undefined%))
(= (evalc $expression &self)
   (eval $expression))

;Quoting
(: unquote (-> Expression %Undefined%))
(= (unquote (quote $A))
   (let $temp (cut) (eval $A)))

(: noreduce-eq (-> Expression Expression Bool))
(= (noreduce-eq $A $B)
   (== $A $B))

;Types
(= (is-function (-> $A $B)) (let $temp (cut) True))
(= (is-function $X) False)

(= (get-type-space &self $x)
   (get-type $x))
