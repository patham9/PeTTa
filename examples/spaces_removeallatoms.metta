!(import! &self (library lib_spaces))

(friend tim tom)

(= (f $x) 42)

!(remove-all-atoms &self)

!(test (repr (remove-all-atoms &self))
       "(remove-all-atoms &self)")

!(test (repr (f 42))
       "(f 42)")

!(test (collapse (get-atoms &self))
       ())
