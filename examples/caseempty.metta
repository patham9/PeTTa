(= (wu) (case (empty)
              ((1 2)
               (Empty 42))))

(= (f) 42)

(= (wu2) (case (f)
               ((42 ok)
                (Empty nok))))

!(test (wu) 42)
!(test (wu2) ok)
