!(import! &self ../lib/lib_metta4)

!(test (progn (add-atom &self (friend sam tom))
              (remove-atom &self (friend sam tom))
              (add-atom &self (friend sam tim))
              (match &self (friend sam $1) $1))
       tim)

!(test (prog1 1 2 3) 1)
!(test (progn 1 2 3) 3)
