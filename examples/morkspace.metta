;generator: (separate non-det return of each item)
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))
                                      ;Add K atoms, then match 300000's times for friends relations:
!(let $temp (let $x (range 0 10000000);K=10000000  takes 48s    with MORK,   7s with predicate store
                                      ;K=50000000  takes 3m32s  with MORK,  36s with predicate store
                                      ;K=60000000  takes 4m5s   with MORK, 1m5s with predicate store
                                      ;K=100000000 takes 6m24s  with MORK, out of mem with predicate store
                                      ;K=200000000 takes 14m50s with MORK, out of mem with predicate store
                                      ;K=300000000 takes 20m2s  with MORK, out of mem with predicate store
                                      ;K=400000000 takes 28m21  with MORK, out of mem with predicate store
                                      ;K=500000000 out of mem   with MORK, out of mem with predicate store
                 (add-atom &mork (friend sam $x)))
      (empty))


!(let $temp (let $x (range 1 100000)
                 (match &mork (friend $y $x) (friend $y $x)))
      (empty))

!(let $temp (let $x (range 1 100000)
                 (match &mork (friend $y $x) (friend $y $x)))
      (empty))

!(let $temp (let $x (range 1 100000)
                 (match &mork (friend $y $x) (friend $y $x)))
      (empty))

!(remove-atom &mork (friend sam 42))
!(test (collapse (match &mork (friend sam 42) (friend sam 42))) ())
