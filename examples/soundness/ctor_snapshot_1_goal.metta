; Multi-file soundness case, part 1 of 2 - load this file FIRST.
;
; This is the constructor-set SNAPSHOT limitation documented on
; union_member_excluded/3, isolated so it can be regression-tested. The second
; branch of the case below narrows the union by ELEMENT COUNT only: its pattern
; has a variable head, so it carries no tag evidence. That narrowing is admitted
; because branch one already consumed CPU, and CPU is - right now, in this file -
; Goal's only four-element constructor.
;
; "Right now" is the whole problem. The exclusion is a snapshot of the
; constructor set as it stands while this file compiles. Part 2 declares a
; SECOND four-element Goal constructor, and nothing revisits this clause.
;
; See examples/multifile/ctor_snapshot_2_gpu.metta.

(: Goal Type)
(: CPU (-> Symbol Expression Expression Goal))

(: Statement Type)
(: Implication Statement)
(: KBContext Type)
(: mkctx (-> Symbol Symbol KBContext))
(: Proof (Newtype Expression))

(: proof-weight (-> Proof Number Number))
(= (proof-weight $_p $n) (+ $n 1))

(: cpu-conf (-> Symbol Expression Expression Number))
(= (cpu-conf $_f $_a $_r) 0.2)

(: term-conf
   (-> (| Goal
          (Statement KBContext Proof Number))
       Number))
(= (term-conf $t)
   (case $t
     (((CPU $fun $args $res) (cpu-conf $fun $args $res))
      (($_type ($_kbid $_ctx $_vars) $prf $w) (proof-weight $prf $w)))))

; The honest call: a real positional member of the second union arm.
!(test (term-conf (Implication (mkctx a b) (brand Proof (p)) 41)) 42)
