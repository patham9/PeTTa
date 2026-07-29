; A late declaration makes the second old clause invalid. Revalidation stages
; both replacements before erasing either old clause, so catching the error
; leaves the complete prior executable clause set rather than only clause b.
(= (staged-choice a) 1)
(= (staged-choice b) "old")

!(let $ignored
   (catch
      (eval
         (quote
            (add-atom &self
               (: staged-choice (-[det]-> Atom Number))))))
   ; Remove the failed late declaration before observing the function. This
   ; also makes the example oracle-safe; if the old eager recompile had erased
   ; clause a before failing on b, this removal could only rebuild b.
   (let $removed
      (remove-atom &self
         (: staged-choice (-[det]-> Atom Number)))
      (test (collapse (staged-choice $which)) (1 "old"))))
