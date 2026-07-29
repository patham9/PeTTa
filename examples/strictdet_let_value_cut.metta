(: let-cut-choice (-[det]-> Atom Atom))
(= (let-cut-choice $value) (let False (cut) a))
(= (let-cut-choice a) a)

!(test (collapse (let-cut-choice a)) (a))
