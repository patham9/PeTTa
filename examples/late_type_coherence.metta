; A caller sees a type declaration even when the declaration appears later.
(= (typed-through $x) (typed-identity $x))
(: typed-identity (-> Number Number))
(= (typed-identity $x) $x)

!(test (collapse (typed-through 7)) (7))
!(test (collapse (typed-through "bad")) ())

; Adding and removing a type updates callers that are already compiled.
(= (dynamic-identity $x) $x)
(= (dynamic-through $x) (dynamic-identity $x))

!(test (collapse (dynamic-through "before")) ("before"))
!(add-atom &self (: dynamic-identity (-> Number Number)))
!(test (collapse (dynamic-through "after-add")) ())
!(remove-atom &self (: dynamic-identity (-> Number Number)))
!(test (collapse (dynamic-through "after-remove")) ("after-remove"))
