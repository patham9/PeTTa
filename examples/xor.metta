(= (check_xor $source $destination)
   (if (xor (== $source $destination)
            (> $source $destination))
            42
            0))

!(test (check_xor 2 2) 42)
!(test (check_xor 4 2) 42)
