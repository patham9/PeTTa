;h just expects a structure:
(= (h (justdata haha $B) $C)
   (+ $B $C))

;just structure matching (check out listhead.metta for list structure matching)
!(test (h (justdata haha 30) 40) 70)

;Please see listhead.metta next!
