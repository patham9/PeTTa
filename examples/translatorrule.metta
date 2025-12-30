(= (runtime42 $arg)
   (cons 42 $arg))

(= (compileeval42 $arg)
   (cons 42 $arg))

(= (compile42 $arg)
   (quote (cons 42 $arg)))

!(add-translator-rule! compileeval42)
!(add-translator-rule! compile42)

!(test (runtime42 (43)) (42 43))
!(test (compileeval42 (43)) (42 43))
!(test (compile42 (43)) (42 43))
