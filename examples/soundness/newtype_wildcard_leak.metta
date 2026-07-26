; --oracle probe for the (Newtype T)-over-a-wildcard hole.
;
; type_unify/2 rewrites a brand to its representation, and Expression is a
; wildcard that unifies with everything. So (: Proof (Newtype Expression))
; makes Proof and Number mutually compatible: leak's body hands a Proof to a
; function that requires a Number and the obligation is discharged statically,
; with no guard emitted. The program compiles clean, in --strict too.
;
; The extended argument oracle re-checks that discharged obligation where it
; was made, and reports the certification the value contradicts instead of the
; Prolog arithmetic error the program would otherwise die of.
;
; PARTIAL: the oracle only sees this because the leaked payload is a String,
; which check_value/3 can definitely reject. Brand a list instead - (brand
; Proof (a b)) - and the oracle agrees with the certification, because it uses
; the checker's own value relation and that relation has the same hole. The
; converse direction (a raw String accepted where Proof is required) is
; invisible for the same reason. See the KNOWN LIMITATION on oracle_check/2.
(: Proof (Newtype Expression))

(: need-number (-> Number Number))
(= (need-number $n) (+ $n 1))

(: leak (-> Proof Number))
(= (leak $p) (need-number $p))

!(test (leak (brand Proof "not a number")) 0)
