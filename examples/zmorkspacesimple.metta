!(import! &self ../lib/lib_mm2)

!(＋ (friend sam tim))
!(~> (, (friend sam $x))
        (O (- (friend sam $x))
           (+ (enemy sam $x))))
!(test (? (enemy sam $x) $x)
       tim)
