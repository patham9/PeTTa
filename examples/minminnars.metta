(= (clamp $v $min $max)
   (min $max (max $v $min)))

;; Truth functions
(= (Truth_c2w $c)
   (/ $c (- 1 $c)))

(= (Truth_w2c $w)
   (/ $w (+ $w 1)))

(= (Truth_Deduction (stv $f1 $c1)
                    (stv $f2 $c2))
   (stv (* $f1 $f2) (* (* $f1 $f2) (* $c1 $c2))))

(= (Truth_Abduction (stv $f1 $c1) 
                    (stv $f2 $c2))
   (stv $f2 (Truth_w2c (* (* $f1 $c1) $c2))))

(= (Truth_Induction $T1 $T2)
   (Truth_Abduction $T2 $T1))

(= (Truth_Exemplification (stv $f1 stv $c1) 
                          (stv $f2 $c2))
   (stv 1.0 (Truth_w2c (* (* $f1 $f2) (* $c1 $c2)))))

(= (Truth_StructuralDeduction $T)
   (Truth_Deduction $T (stv 1.0 0.9)))
  
(= (Truth_Negation (stv $f $c))
   (stv (- 1 $f) $c))

(= (Truth_StructuralDeductionNegated $T)
   (Truth_Negation (Truth_StructuralDeduction $T)))

(= (Truth_Intersection (stv $f1 $c1) 
                       (stv $f2 $c2))
   (stv (* $f1 $f2) (* $c1 $c2)))

(= (Truth_StructuralIntersection $T)
   (Truth_Intersection $T (stv 1.0 0.9)))

(= (Truth_or $a $b)
   (- 1 (* (- 1 $a) (- 1 $b))))

(= (Truth_Comparison (stv $f1 $c1)
                     (stv $f2 $c2))
   (let $f0 (Truth_or $f1 $f2) 
        (stv (if (== $f0 0.0)
                 0.0 
                 (/ (* $f1 $f2) $f0))
             (Truth_w2c (* $f0 (* $c1 $c2))))))
  
(= (Truth_Analogy (stv $f1 $c1)
                  (stv $f2 $c2))
   (stv (* $f1 $f2) (* (* $c1 $c2) $f2)))

(= (Truth_Resemblance (stv $f1 $c1)
                      (stv $f2 $c2))
   (stv (* $f1 $f2) (* (* $c1 $c2) (Truth_or $f1 $f2))))

(= (Truth_Union (stv $f1 $c1)
                (stv $f2 $c2))
   ((Truth_or $f1 $f2) (* $c1 $c2)))

(= (Truth_Difference (stv $f1 $c1)
                     (stv $f2 $c2))
   (stv (* $f1 (- 1 $f2)) (* $c1 $c2)))

(= (Truth_DecomposePNN (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $fn (* $f1 (- 1 $f2))
        (stv (- 1 $fn) (* $fn (* $c1 $c2)))))

(= (Truth_DecomposeNPP (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $f (* (- 1 $f1) $f2)
        (stv $f (* $f (* $c1 $c2)))))

(= (Truth_DecomposePNP (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $f (* $f1 (- 1 $f2))
        (stv $f (* $f (* $c1 $c2)))))

(= (Truth_DecomposePPP $v1 $v2)
   (Truth_DecomposeNPP (Truth_Negation $v1) $v2))

(= (Truth_DecomposeNNN (stv $f1 $c1)
                       (stv $f2 $c2))
   (let $fn (* (- 1 $f1) (- 1 $f2))
        ((- 1 $fn) (* $fn (* $c1 $c2)))))

(= (Truth_Eternalize (stv $f $c))
   (stv $f (Truth_w2c $c)))

(= (Truth_Revision (stv $f1 $c1)
                   (stv $f2 $c2))
   (let* (($w1 (Truth_c2w $c1))
          ($w2 (Truth_c2w $c2))
          ($w  (+ $w1 $w2))
          ($f (/ (+ (* $w1 $f1) (* $w2 $f2)) $w))
          ($c (Truth_w2c $w)))
          (stv (min 1.00 $f) (min 0.99 (max (max $c $c1) $c2)))))

(= (Truth_Expectation (stv $f $c))
   (+ (* $c (- $f 0.5)) 0.5))

;; INFERENCE RULES

;;NAL-1
;;!Revision
(= (|- ($T $T1) ($T $T2)) ($T (Truth_Revision $T1 $T2)))
;;!Syllogistic rules for Inheritance:
(= (|- ((Inheritance $a $b) $T1) ((Inheritance $b $c) $T2)) ((Inheritance $a $c) (Truth_Deduction $T1 $T2)))
(= (|- ((Inheritance $a $b) $T1) ((Inheritance $a $c) $T2)) ((Inheritance $c $b) (Truth_Induction $T1 $T2)))
(= (|- ((Inheritance $a $c) $T1) ((Inheritance $b $c) $T2)) ((Inheritance $b $a) (Truth_Abduction $T1 $T2)))
(= (|- ((Inheritance $a $b) $T1) ((Inheritance $b $c) $T2)) ((Inheritance $c $a) (Truth_Exemplification $T1 $T2)))

;;NAL-3
;;!Set decomposition:
(= (|- ((Inheritance (ExtSet $A $B) $M) $T)) ((Inheritance (ExtSet $A) $M) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance (ExtSet $A $B) $M) $T)) ((Inheritance (ExtSet $B) $M) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance $M (IntSet $A $B)) $T)) ((Inheritance $M (IntSet $A)) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance $M (IntSet $A $B)) $T)) ((Inheritance $M (IntSet $B)) (Truth_StructuralDeduction $T)))
;;!Extensional and intensional intersection decomposition:
(= (|- ((Inheritance (IntInt $S $P) $M) $T)) ((Inheritance $S $M) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance $M (ExtInt $S $P)) $T)) ((Inheritance $M $S) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance (IntInt $S $P) $M) $T)) ((Inheritance $P $M) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance $M (ExtInt $S $P)) $T)) ((Inheritance $M $P) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance (IntDiff $A $S) $M) $T)) ((Inheritance $A $M) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance $M (ExtDiff $B $S)) $T)) ((Inheritance $M $B) (Truth_StructuralDeduction $T)))
(= (|- ((Inheritance (IntDiff $A $S) $M) $T)) ((Inheritance $S $M) (Truth_StructuralDeductionNegated $T)))
(= (|- ((Inheritance $M (ExtDiff $B $S)) $T)) ((Inheritance $M $S) (Truth_StructuralDeductionNegated $T)))
;;!Extensional and intensional intersection decomposition:
(= (|- ((Inheritance $S $M) $T1) ((Inheritance (IntInt $S $P) $M) $T2)) ((Inheritance $P $M) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((Inheritance $P $M) $T1) ((Inheritance (IntInt $S $P) $M) $T2)) ((Inheritance $S $M) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((Inheritance $S $M) $T1) ((Inheritance (ExtInt $S $P) $M) $T2)) ((Inheritance $P $M) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((Inheritance $P $M) $T1) ((Inheritance (ExtInt $S $P) $M) $T2)) ((Inheritance $S $M) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((Inheritance $S $M) $T1) ((Inheritance (IntDiff $S $P) $M) $T2)) ((Inheritance $P $M) (Truth_DecomposePNP $T1 $T2)))
(= (|- ((Inheritance $S $M) $T1) ((Inheritance (IntDiff $P $S) $M) $T2)) ((Inheritance $P $M) (Truth_DecomposeNNN $T1 $T2)))
(= (|- ((Inheritance $M $S) $T1) ((Inheritance $M (ExtInt $S $P)) $T2)) ((Inheritance $M $P) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((Inheritance $M $P) $T1) ((Inheritance $M (ExtInt $S $P)) $T2)) ((Inheritance $M $S) (Truth_DecomposePNN $T1 $T2)))
(= (|- ((Inheritance $M $S) $T1) ((Inheritance $M (IntInt $S $P)) $T2)) ((Inheritance $M $P) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((Inheritance $M $P) $T1) ((Inheritance $M (IntInt $S $P)) $T2)) ((Inheritance $M $S) (Truth_DecomposeNPP $T1 $T2)))
(= (|- ((Inheritance $M $S) $T1) ((Inheritance $M (ExtDiff $S $P)) $T2)) ((Inheritance $M $P) (Truth_DecomposePNP $T1 $T2)))
(= (|- ((Inheritance $M $S) $T1) ((Inheritance $M (ExtDiff $P $S)) $T2)) ((Inheritance $M $P) (Truth_DecomposeNNN $T1 $T2)))

;; NAL-4
;;!Rules for more efficient reasoning about relation components:
(= (|- ((Inheritance (Product $A $B) $R) $T1) ((Inheritance (Product $C $B) $R) $T2)) ((Inheritance $C $A) (Truth_Abduction $T1 $T2)))
(= (|- ((Inheritance (Product $A $B) $R) $T1) ((Inheritance (Product $A $C) $R) $T2)) ((Inheritance $C $B) (Truth_Abduction $T1 $T2)))
(= (|- ((Inheritance $R (Product $A $B)) $T1) ((Inheritance $R (Product $C $B)) $T2)) ((Inheritance $C $A) (Truth_Induction $T1 $T2)))
(= (|- ((Inheritance $R (Product $A $B)) $T1) ((Inheritance $R (Product $A $C)) $T2)) ((Inheritance $C $B) (Truth_Induction $T1 $T2)))
(= (|- ((Inheritance (Product $A $B) $R) $T1) ((Inheritance $C $A) $T2)) ((Inheritance (Product $C $B) $R) (Truth_Deduction $T1 $T2)))
(= (|- ((Inheritance (Product $A $B) $R) $T1) ((Inheritance $A $C) $T2)) ((Inheritance (Product $C $B) $R) (Truth_Induction $T1 $T2)))
(= (|- ((Inheritance (Product $A $B) $R) $T1) ((Inheritance $C $B) $T2)) ((Inheritance (Product $A $C) $R) (Truth_Deduction $T1 $T2)))
(= (|- ((Inheritance (Product $A $B) $R) $T1) ((Inheritance $B $C) $T2)) ((Inheritance (Product $A $C) $R) (Truth_Induction $T1 $T2)))
(= (|- ((Inheritance $R (Product $A $B)) $T1) ((Inheritance $A $C) $T2)) ((Inheritance $R (Product $C $B)) (Truth_Deduction $T1 $T2)))
(= (|- ((Inheritance $R (Product $A $B)) $T1) ((Inheritance $C $A) $T2)) ((Inheritance $R (Product $C $B)) (Truth_Abduction $T1 $T2)))
(= (|- ((Inheritance $R (Product $A $B)) $T1) ((Inheritance $B $C) $T2)) ((Inheritance $R (Product $A $C)) (Truth_Deduction $T1 $T2)))
(= (|- ((Inheritance $R (Product $A $B)) $T1) ((Inheritance $C $B) $T2)) ((Inheritance $R (Product $A $C)) (Truth_Abduction $T1 $T2)))

;;NAL-5
;;!Negation conjunction and disjunction decomposition:
(= (|- ((Not $A) $T)) ($A (Truth_Negation $T)))
(= (|- ((Conjunction $A $B) $T)) ($A (Truth_StructuralDeduction $T)))
(= (|- ((Conjunction $A $B) $T)) ($B (Truth_StructuralDeduction $T)))
(= (|- ($S $T1) ((Conjunction $S $A) $T2)) ($A (Truth_DecomposePNN $T1 $T2)))
(= (|- ($S $T1) ((Disjunction $S $A) $T2)) ($A (Truth_DecomposeNPP $T1 $T2)))
(= (|- ($S $T1) ((Conjunction (Not $S) $A) $T2)) ($A (Truth_DecomposeNNN $T1 $T2)))
(= (|- ($S $T1) ((Disjunction (Not $S) $A) $T2)) ($A (Truth_DecomposePPP $T1 $T2)))
;;!Higher-order decomposition
(= (|- ($A $T1) ((Implication $A $B) $T2)) ($B (Truth_Deduction $T1 $T2)))
(= (|- ($A $T1) ((Implication (Conjunction $A $B) $C) $T2)) ((Implication $B $C) (Truth_Deduction $T1 $T2)))
(= (|- ($B $T1) ((Implication $A $B) $T2)) ($A (Truth_Abduction $T1 $T2)))

;;How many derivation steps (task selections)
(= (PLN.Config.MaxSteps) 100)

;;Size of active tasks PQ
(= (PLN.Config.TaskQueueSize) 10)

;;Max beliefs buffer size
(= (PLN.Config.BeliefQueueSize) 100)

;;Whether evidence was just counted once
(= (StampDisjoint $Ev1 $Ev2)
   (== () (collapse (let* (($x (superpose $Ev1))
                           ($y (superpose $Ev2)))
                          (case (== $x $y) ((True True)))))))

;;Concat stamp with sorting
(= (StampConcat $stamp $addition)
   (if (== $addition ())
       $stamp
       (msort (append $stamp $addition))))

;;retrieve the best candidate (PQ functionality)
(= (BestCandidate $evaluateCandidateFunction $bestCandidate $tuple)
   (if (== $tuple ())
       $bestCandidate
       (let* (($head (car-atom $tuple))
              ($tail (cdr-atom $tuple)))
             (if (> ($evaluateCandidateFunction $head)
                    ($evaluateCandidateFunction $bestCandidate))
                 (BestCandidate $evaluateCandidateFunction $head $tail)
                 (BestCandidate $evaluateCandidateFunction $bestCandidate $tail)))))

;;candidate evaluation based on priority
(= (PriorityRank (Sentence ($x (stv $f $c)) $Ev1)) $c)
(= (PriorityRank ()) -99999.0)

;;candidate elimination based on negated priority
(= (PriorityRankNeg (Sentence ($x (stv $f $c)) $Ev1)) (- 0.0 $c))
(= (PriorityRankNeg ()) -99999.0)

;;Return limited-sized version of $L (bounded PQ functionality)
(= (LimitSize $L $size)
   (if (< (length $L) $size)
       $L
       (let $lowestPriorityItem (BestCandidate PriorityRankNeg () $L)
            (LimitSize (exclude-item $lowestPriorityItem $L) $size))))

;;Priority-queue based task ranking deriver with belief buffer
(= (PLN.Derive $Tasks $Beliefs $steps $maxsteps $taskqueuesize $beliefqueuesize)
   (if (or (> $steps $maxsteps) (== $Tasks ()))
       ($Tasks $Beliefs)
       (let (Sentence $x $Ev1) (BestCandidate PriorityRank () $Tasks)
            (let $derivations
                 (collapse (superpose ((let* (((Sentence $y $Ev2) (superpose $Beliefs))
                                              ($stamp (msort (append $Ev1 $Ev2))))
                                             (if (StampDisjoint $Ev1 $Ev2)
                                                 (case (superpose ((|- $x $y)
                                                                   (|- $y $x)))
                                                       ((($T $TV) (Sentence ($T $TV) $stamp))))
                                                 (empty)))
                                       (case (|- $x) ((($T3 $TV3) (Sentence ($T3 $TV3) $Ev1)))))))
                 (let $temp (trace! (SELECTED $steps (Sentence $x $Ev1)) 42)
                            (PLN.Derive (LimitSize (exclude-item (Sentence $x $Ev1) (list_to_set (append $Tasks $derivations))) $taskqueuesize)
                                        (LimitSize (list_to_set (append $Beliefs $derivations)) $beliefqueuesize)
                                        (+ $steps 1)
                                        $maxsteps
                                        $taskqueuesize
                                        $beliefqueuesize))))))

(= (PLN.Derive $Tasks $Beliefs $maxsteps $taskqueuesize $beliefqueuesize)
   (PLN.Derive $Tasks $Beliefs 1 $maxsteps $taskqueuesize $beliefqueuesize))

(= (PLN.Derive $Tasks $Beliefs $maxsteps)
   (PLN.Derive $Tasks $Beliefs $maxsteps (PLN.Config.TaskQueueSize) (PLN.Config.BeliefQueueSize)))

(= (PLN.Derive $Tasks $Beliefs)
   (PLN.Derive $Tasks $Beliefs (PLN.Config.MaxSteps)))

;;Candidate evaluation based on confidence
(= (ConfidenceRank ((stv $f $c) $Ev)) $c)
(= (ConfidenceRank ()) 0)

;;Pose a question of a certain term to the system on some knowledge base
(= (PLN.Query $Tasks $Beliefs $term $maxsteps $taskqueuesize $beliefqueuesize)
   (BestCandidate ConfidenceRank () (collapse (let ($TasksRet $BeliefsRet) (PLN.Derive $Tasks $Beliefs $maxsteps $taskqueuesize $beliefqueuesize)
                                                   (case (superpose $BeliefsRet)
                                                         (((Sentence ($Term $TV) $Ev) (case (== $Term $term)
                                                                                            ((True ($TV $Ev)))))))))))

(= (PLN.Query $kb $term $maxsteps $taskqueuesize $beliefqueuesize)
   (PLN.Query $kb $kb $term $maxsteps $taskqueuesize $beliefqueuesize))

(= (PLN.Query $kb $term $maxsteps)
   (PLN.Query $kb $term $maxsteps (PLN.Config.TaskQueueSize) (PLN.Config.BeliefQueueSize))) ;default space bound

(= (PLN.Query $kb $term)
   (PLN.Query $kb $term (PLN.Config.MaxSteps))) ;default steps bound

(= (kb)
   ((Sentence ((Implication (Inheritance (Product $1 $2) friend)
                            (Implication (Inheritance $1
                                                      (IntSet smokes))
                                         (Inheritance $2
                                                      (IntSet smokes))))
               (stv 0.4 0.9)) (1))
    (Sentence ((Implication (Inheritance $1
                                         (IntSet smokes))
                            (Inheritance $1
                                         (IntSet cancerous)))
               (stv 0.6 0.9)) (2))
    (Sentence ((Inheritance (Product Anna Bob)
                            friend) 
               (stv 1.0 0.9)) (3))
    (Sentence ((Inheritance (Product Anna Edward)
                            friend)
               (stv 1.0 0.9)) (4))
    (Sentence ((Inheritance (Product Anna Frank)
                            friend)
               (stv 1.0 0.9)) (5))
    (Sentence ((Inheritance (Product Edward Frank)
                            friend)
               (stv 1.0 0.9)) (6))
    (Sentence ((Inheritance (Product Gary Helen)
                            friend)
               (stv 1.0 0.9)) (7))
    (Sentence ((Inheritance (Product Gary Frank)
                            friend)
               (stv 0.0 0.9)) (8))
    (Sentence ((Inheritance Anna
                            (IntSet smokes))
               (stv 1.0 0.9)) (9))
    (Sentence ((Inheritance Edward
                            (IntSet smokes))
               (stv 1.0 0.9)) (10))))

!(test (PLN.Query (kb)
                  (Inheritance Edward
                               (IntSet cancerous)))
       ((stv 0.6 0.48941156079382964) (2 5 6 9 10)))

