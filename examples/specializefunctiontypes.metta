(= (g $x) $x)

(: f (-> Atom Number Atom))
(: f (-> Atom String Atom))
(= (f $g $x)
   (repra ($g $x)))

!(f g 42)

!(test (match &self (: f_Spec_[g] (-> Atom Number Atom)) ok) ok)
!(test (match &self (: f_Spec_[g] (-> Atom String Atom)) ok) ok)
