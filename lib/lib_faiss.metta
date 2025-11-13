!(import! &self ../lib/lib_prolog)

!(import_prolog_functions_from_file "/home/nartech/PeTTa/faiss_ffi/faiss.pl" (faiss_create faiss_add faiss_search faiss_remove))
!(import_prolog_functions_from_file "/home/nartech/PeTTa/faiss_ffi/embed.pl" (embed))

;Create a new atom-tagged vectorspace
(= (new-atom-vectorspace $space $dimension)
   (bind! $space (new-state ((faiss_create $dimension) $dimension))))

;Custom vector:
(= (add-atom-vector $space $atom $vector)
   (let ($vdb $dim) (get-state $space)
        (faiss_add $vdb (($atom $vector)))))

;Structural Random Indexing
(= (add-atom-SRI $space $atom)
   (let ($vdb $dim) (get-state $space)
        (faiss_add $vdb (($atom (embed $atom $dim))))))

;Match in the space by similarity
(= (match-k $k $space $query)
   (faiss_search (car-atom (get-state $space))
                 $query $k))

;Match using structural random embedding:
(= (match-sri $k $space $query)
   (let ($vdb $dim) (get-state $space)
        (faiss_search $vdb (embed $query $dim) $k)))

;Helper to extract atom from closest-match results
(= (atom-of $x) (car-atom (car-atom $x)))
