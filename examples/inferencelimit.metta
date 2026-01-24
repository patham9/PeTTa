!(import_prolog_function statistics)
!(import_prolog_function reset)

(= (BudgetLimitCheck)
   (if (> (- (statistics inferences) (get-state &initinferences)) (get-state &inferenceslimit))
       (translatePredicate (shift (pay-for-inferences)))
       True))

(= (AssignBudgetLimit $N)
   (progn (change-state! &initinferences (statistics inferences))
          (change-state! &inferenceslimit $N)))

(= (InjectBudgetLimitCheck)
   (let $L (collapse (match &self $option
           (if (= $option (quote (= $head $body)))
                (let (cons $f $rest) $head
                     (if (not (is-member $f (quote (BudgetLimitCheck AssignBudgetLimit InjectBudgetLimitCheck CallWithBudgetLimitInternal CallWithBudgetLimit))))
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
                                   (CallWithBudgetLimitInternal $var $cont))
                            (Ok bye)))))))

(: CallWithBudgetLimit (-> Expression Atom))
(= (CallWithBudgetLimit $G)
   (let $ext (Predicate (union-atom $G ($X)))
        (CallWithBudgetLimitInternal $X $ext)))

;;;;;;;;;;; EXAMPLE:

;1. Let user define MeTTa code to execute:
(= (fib $N)
   (if (< $N 2)
       $N
       (+ (fib (- $N 1))
          (fib (- $N 2)))))

;2. Inject compute budget check into user code:
!(InjectBudgetLimitCheck)

;3. Assign a budget limit to the user:
!(AssignBudgetLimit 10000)

;Call function while allowing the user to buy more inferences to continue computation when running out of inferences:
!(CallWithBudgetLimit (fib 15))
