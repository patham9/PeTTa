;First take a look at constanthead.metta and listhead.metta
;Now we generalize the idea. Let's first just have a function:
(= (myfunc $A $B)
   (append (append (42) $A) $B))

;Consider the following,
;we want to constrain an argument to be the return value of a function call:
;(see invertfunction.metta now to see that in pure form)
(= (h_old $A $C)
   (if (= $A (myfunc (10) $B)) ;= is also unification in PeTTa
       ($B $C)
       (empty)))

;Assuming we can arbitrarily parametrize functions (e.g. invert them as a special case)
;we can also constrain input argument values to correspond to the return value:
;thereby not only constraining what input arguments qualify to call the function
;but also constraining variables therein establishing a relational dependency:
(= (h (myfunc (10) $B) $C)
   ($B $C))

;lets try:
!(test (h (42 10 40) 42000)
       ((40) 42000))
;same as in the old formulation:
!(test (h_old (42 10 40) 42000)
       ((40) 42000))

;Check out functionhead2.metta!
