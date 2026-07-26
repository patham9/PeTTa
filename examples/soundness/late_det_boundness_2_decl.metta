; Part 2 of 2 - load AFTER late_det_boundness_1_defs.metta.
;
; The -[det]-> arrow arrives after f was compiled. enforce_late_declaration
; recompiles f's clause through translate_clause with the declaration in place,
; which RE-EMITS the boundness check (src/translator.pl, det_boundness_checks).
; This cannot be reproduced in one file: the per-file declaration prepass makes
; a file's own arrow declarations visible to every definition in it, so the
; check would have been emitted on the first compile.
;
; !(leak) now hits the re-emitted check and throws unbound_det_argument, where
; before the declaration it returned an unbound result unchecked - enforced, or
; rejected, never believed unchecked.
(: f (-[det]-> Number Number))

!(leak)
