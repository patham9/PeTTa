;makes example fail without MORK:
!(mm2-exec &mork 1)

;generator: (separate non-det return of each item)
(= (range $K $N)
   (if (< $K $N)
       (superpose ($K (range (+ $K 1) $N)))
       (empty)))

!(let $temp (let $x (range 1 100000)
                 (add-atom &mork (friend sam $x)))
      (empty))

!(add-atom &mork (exec 0 (, (friend sam $x))
                         (O (- (friend sam $x))
                            (+ (freund sam $x)))))

!(mm2-exec &mork 1)

!(test (length (collapse (match &mork (freund sam $x) (freund sam $x)))) 99999)

!(test (collapse (let (freund sam $u) (match &mork (freund sam $x) (freund sam $x))
                      (if (> $u 99994)
                          $u
                          (empty))))
       (99995 99996 99997 99998 99999))
