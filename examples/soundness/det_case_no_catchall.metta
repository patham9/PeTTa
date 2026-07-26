; --oracle-det probe: a -[det]-> function whose case has no catch-all.
;
; det means EXACTLY ONE solution. (f 3) matches neither case branch, so it has
; zero. The static exhaustiveness check (det_exhaustiveness_prepass/1) reasons
; about CLAUSE HEADS, and this function has a single clause with a variable
; head - it is exhaustive at the clause level and non-exhaustive inside its
; body, which nothing looks at.
;
; Without --oracle-det the call simply fails: (collapse (f 3)) is (), the
; program says nothing is wrong, and a det caller downstream silently
; disappears. Under --oracle-det the call site counts zero solutions for a
; function that promised one, and throws.
(: f (-[det]-> Number Number))
(= (f $x) (case $x ((1 10) (2 20))))
!(test (collapse (f 3)) ())
