; The runtime side of the boundary check. leak hands a -[det]-> function an
; UNBOUND argument; enforcement throws a clear unbound_det_argument error where
; before the code enumerated a finite type or crashed downstream in a builtin.
; Default mode: enforcement is mode-independent, keyed on the explicit arrow.
(: use-it (-[det]-> Number Number))
(= (use-it $x) (+ $x 1))

(: leak (-> Number))
(= (leak) (use-it $u))

!(leak)
