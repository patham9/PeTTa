; Atom parameters preserve ordinary call syntax, but explicitly declared data
; producers and contextual accessors supply their computed values.
(: produced-expression (-[det]-> Expression))
(= (produced-expression) (quote (payload value)))

(: produced-atom (-[det]-> Atom))
(= (produced-atom) payload)

(: payload-expression? (-[det]-> Atom Bool))
(= (payload-expression? $value)
   (== (car-atom $value) payload))

(: payload-atom? (-[det]-> Atom Bool))
(= (payload-atom? $value) (== $value payload))

(: first-payload? (-[det]-> Atom Bool))
(= (first-payload? $value)
   (payload-atom? (car-atom $value)))

(: tail-first-payload? (-[det]-> Atom Bool))
(= (tail-first-payload? $value)
   (first-payload? (cdr-atom $value)))

!(test (payload-expression? (produced-expression)) true)
!(test (payload-atom? (produced-atom)) true)
!(test (first-payload? (payload other)) true)
!(test (tail-first-payload? (other payload value)) true)
