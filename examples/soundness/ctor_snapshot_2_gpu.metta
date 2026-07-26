; Multi-file soundness case, part 2 of 2 - load this file AFTER
; ctor_snapshot_1_goal.metta.
;
; A second four-element Goal constructor, arriving too late for the exclusion
; that term-conf's compilation already made. A GPU value now reaches the
; count-only branch, where the checker certified $w : Number - and GPU's third
; field is a String.
;
; Nothing here is rejected at compile time. Without --oracle the program
; reaches (+ "oops" 1) and dies inside is/2 with a Prolog arithmetic error that
; names neither the certification nor the type. Under --oracle the argument
; certification is re-checked where it was made and the call site itself
; reports the contradiction.

(: GPU (-> Expression Expression String Goal))

!(test (term-conf (GPU (a b c) (p) "oops")) 0)
