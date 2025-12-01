!(import! &self lib_faiss)

;providing our own embeddings:
!(new-atom-vectorspace &avs 4)
!(add-atom-vector &avs a (0.1 0.2 0.3 0.4))
!(add-atom-vector &avs b (0.8 0.9 1.0 1.1))
!(add-atom-vector &avs c (0.4 0.3 0.4 0.8))
!(test (atom-of (match-k 1 &avs (0.09 0.19 0.29 0.39)))
       a)

;cheap structural random indexing for atoms:
!(new-atom-vectorspace &sri 20)
!(add-atom-SRI &sri ((red apple) strudel))
!(add-atom-SRI &sri ((yellow banana) strudel))
!(add-atom-SRI &sri (strudel chaos))
!(test (atom-of (match-sri 1 &sri ((red strawberry) strudel)))
       ((red apple) strudel))
