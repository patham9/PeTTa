!(import! &self (library lib_spaces))
!(import_prolog_function statistics)
!(import_prolog_function reset)

(= (BudgetLimitCheck)
   (if (> (- (statistics inferences) (get-state &initinferences)) (get-state &inferenceslimit))
       (progn ;metadata with clause refs needs to be dropped:
              (translatePredicate (retractall (Predicate (translated_from $1 $2))))
              ;save current atom space with computation continuation entry point:
              (translatePredicate (qsave_program "saveme.p" ((Predicate (goal (Predicate (quote (__resume__ $x))))))))
              ;pause the compute branch returning to reset position:
              (translatePredicate (shift (pay-for-inferences))))
       True))

(= (AssignBudgetLimit $N)
   (progn (change-state! &initinferences (statistics inferences))
          (change-state! &inferenceslimit $N)))

(= (InjectBudgetLimitCheck)
   (let $L (collapse (match &self $option
           (if (= $option (quote (= $head $body)))
                (let (cons $f $rest) $head
                     (if (not (is-member $f (quote (BudgetLimitCheck AssignBudgetLimit InjectBudgetLimitCheck
                                                    CallWithBudgetLimitInternal CallWithBudgetLimit __resume__ resume))))
                         (progn (remove-atom &self $option)
                                $option))))))
        (let $f (superpose $L)
             (if (= $f (quote (= $head $body)))
                 (add-atom &self (= $head (progn (BudgetLimitCheck) $body)))))))

(= (CallWithBudgetLimitInternal $var $predicate)
   (let $cont (reset $predicate $signal)
        (if (is-var $signal)
            $var
            (progn (println! "Out of inferences. Do you want to buy another 10000? Answer (yes) or (no):")
                   (let $response (catch (readln!))
                        (if (== $response (yes))
                            (progn (AssignBudgetLimit 10000)
                                   ;continue computation:
                                   (CallWithBudgetLimitInternal $var $cont))
                            (Ok bye)))))))

(: CallWithBudgetLimit (-> Expression Atom))
(= (CallWithBudgetLimit $G)
   (let $ext (Predicate (union-atom $G ($X)))
        (progn (if (find &self (= (resume) (CallWithBudgetLimit $G)))
                   AlreadyExists
                   (add-atom &self (= (resume) (CallWithBudgetLimit $G))))
               (CallWithBudgetLimitInternal $X $ext))))

;Functions to resume computation with "swipl -x saveme.p":
(= (resume) (empty))
(= (__resume__)
   (prog1 (AssignBudgetLimit 10000)
          (let $results (collapse (resume))
               (let $x (superpose $results)
                       (println! $x)))
          (translatePredicate (halt))))

;;;;;;;;;;; EXAMPLE:

(FIB 0 0)
(FIB 1 1)
(FIB 2 1)

(= (last-fib-index)
   (foldall max (match &self (FIB $i $_) $i) 0))

(= (fib-step $i $a $b)
   (progn (add-atom &self (FIB $i $a))
          (println! (calculated FIB $i $a))
          (fib-step (+ $i 1) $b (+ $a $b))))

(= (fib-at $i)
   (once (match &self (FIB $i $v) $v)))

(= (fib-resume)
   (let* (($i (last-fib-index))
          ($a (fib-at (- $i 1)))
          ($b (fib-at $i))
          ($c (+ $a $b)))
         (fib-step (+ $i 1) $c (+ $b $c))))

;2. Inject compute budget check into user code:
!(InjectBudgetLimitCheck)

;3. Assign a budget limit to the user:
!(AssignBudgetLimit 10000)

!(CallWithBudgetLimit (fib-resume))
