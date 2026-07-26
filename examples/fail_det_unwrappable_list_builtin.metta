; reverse/2, append/3, last/2 and length/2 all INVERT: handed a list argument
; that is partial or unbound they enumerate - every split, every length - and
; length/2 does not terminate at all. Nothing in the compiled program
; establishes that the argument is a proper list. A (List Number) declaration
; is a static claim, and the residual guard that would back it up at run time
; is typecheck_or_error/2, which succeeds on an unbound variable.
;
; So none of them can be det, and a -[det]-> body may not call them. The same
; goes for PeTTa's own size-atom, union-atom, subtraction-atom,
; intersection-atom, exclude-item and alpha-unique-atom, whose non_list/1
; guard is true only of a term that can never BECOME a list and so lets an
; open one through. sort-atom and unique-atom are the exceptions: msort/2 and
; list_to_set/2 raise on an open list rather than enumerating, and an
; exception is not an extra solution.
(: rev (-[det]-> (List Number) (List Number)))
(= (rev $xs) (reverse $xs))

!(rev (1 2 3))
