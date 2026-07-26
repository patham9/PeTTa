!(import! &self (library lib_roman))

; or/2 is nondet: bool/1 enumerates the boolean type on an operand it was not
; given. lib_builtin_types declares (: or (-> Bool Bool Bool)), and under
; --strict-det a plain -> reads as a det commitment - so the declaration used
; to certify or as a deterministic fold accumulator while a DIRECT call to it
; from a det body was rejected. The builtin table outranks the declaration in
; both places now, so this is rejected too.

(: any-true (-> (List Bool) Bool))
(= (any-true $bs) (fold-flat or False $bs))

!(any-true (True False))
