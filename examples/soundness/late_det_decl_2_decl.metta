; Multi-file --oracle-det case, part 2 of 2 - load AFTER late_det_decl_1_defs.metta.
;
; The determinism declaration arrives after the definition it constrains.
; process_metta_string/3's declaration prepass makes a file's declarations
; visible to every definition IN THAT FILE, which is why this cannot be
; reproduced in one file - the overlap check would fire. Across files there is
; no such prepass: pick's clauses are already compiled and validated, and
; nothing re-validates them. The compiler warns, and then believes the
; declaration anyway.
;
; The call below therefore has two solutions for a function declared to have
; exactly one. --no-det-cut cannot see it either: the clauses were compiled
; before the declaration existed, so they never got a commit cut to remove.
; Counting the call's solutions is the only thing that catches it.
(: pick (-[det]-> Number))

!(test (collapse (pick)) (1))
