; Multi-file boundness re-emission case, part 1 of 2 - load this file FIRST.
;
; f is defined with NO type declaration, so its compiled clause carries no
; boundness check - nothing about it is a determinism promise yet. leak hands
; f an UNBOUND argument, which is harmless while f is untyped.
(= (f $x) $x)
(= (leak) (f $u))
