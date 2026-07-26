; --oracle-det probe: a -[det]-> function whose body is (once <semidet call>).
;
; deterministic_expr([once, _], ok) discards its argument entirely. once/1 does
; remove the callee's EXTRA solutions, so the analysis is right that nothing
; nondeterministic escapes - but it also throws away the callee's may_fail, and
; once/1 does not manufacture a solution where there was none. A -[semidet]->
; callee with no matching clause leaves (h 3) with zero solutions while h is
; declared -[det]->.
(: lookup (-[semidet]-> Number Number))
(= (lookup 1) 10)

(: h (-[det]-> Number Number))
(= (h $x) (once (lookup $x)))

; the honest call:
!(test (h 1) 10)
; the one that has no solution at all:
!(test (collapse (h 3)) ())
