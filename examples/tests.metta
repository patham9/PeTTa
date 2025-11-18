(= (program1 $Y) (let $X $Y (collapse (superpose (12 (+ $X 4))))))

(= (program2 $Y) (let $list (let $L (1 2 3) (collapse (superpose $L))) (superpose $list)))

(= (program3 $x) (if (== $x 2) (let $z (superpose ((if (< $x 10) (superpose ((42 43))) 43))) $z) (let $z 4 $z)))

(= (program4) (collapse ((program1 42) (program2 42) (program3 2))))

!(test (program4)
       (((12 46) 1 (42 43)) ((12 46) 2 (42 43)) ((12 46) 3 (42 43))))
