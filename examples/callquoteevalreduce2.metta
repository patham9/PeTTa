(= (fib $N)
   (if (< $N 2)
       $N
       (+ (fib (- $N 1))
          (fib (- $N 2)))))

(= (myfunc)
   5)

(= (call-fib)
   (call (fib (myfunc))))

(= (quote-fib)
   (quote (fib (myfunc))))

(= (eval-fib)
   (eval (fib (myfunc))))

(= (reduce-fib)
   (reduce (fib (myfunc))))

!(test (fib-call (call-fib)) (fib-call 5))
!(test (fib-quote (quote-fib)) (fib-quote (quote (fib (myfunc)))))
!(test (fib-eval (eval-fib)) (fib-eval 5))
!(test (fib-reduce (reduce-fib)) (fib-reduce 5))
