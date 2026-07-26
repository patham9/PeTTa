; --oracle-det probe: a -[det]-> function whose body is a two-argument if.
;
; (if $b 1) has no else branch, so it produces nothing when $b is false.
; deterministic_expr/2 analyses [if, Cond, Then] with combine_determinism_list/2
; over its subexpressions, which reports how the PARTS behave and never notices
; that the missing else is itself a failure path - no may_fail is produced, so
; -[det]-> is satisfied by an expression that has zero solutions half the time.
;
; This is the same shape as once/1 and a catch-all-less case: see
; det_once_semidet.metta.
(: g (-[det]-> Bool Number))
(= (g $b) (if $b 1))
!(test (collapse (g false)) ())
