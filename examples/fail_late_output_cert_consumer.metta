; rows initially has a consumed proper-list output certificate, making the
; map-atom call deterministic.  Its late non-list clause withdraws that proof;
; the dependency graph must recompile consume and reject its stale verdict.
(: cert-id (-[det]-> Atom Atom))
(= (cert-id $x) $x)

(: rows (-> Atom Expression))
(= (rows $x) ())

(: consume (-[det]-> Atom Expression))
(= (consume $x) (map-atom (rows $x) cert-id))

!(add-atom &self (= (rows $x) $x))
