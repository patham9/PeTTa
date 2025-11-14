;Create string by appending the list entries
(= (py-str-helper () $outp) $outp)
(= (py-str-helper $L $outp)
   (let* (($head (car-atom $L))
          ($tail (cdr-atom $L))
          ($outp2 (py-call (operator.add $outp (py-call (str $head))))))
     (py-str-helper $tail $outp2)))
(= (py-str $L) (py-str-helper $L ""))

;Evaluate expression via py-call:
(= (py-eval $str)
   (let $dict (py-call (dict))
        (py-call (quote (eval $str $dict $dict)))))

;Add to builtins a function that can call functions via keyword arguments
!(py-eval "setattr(__import__('builtins'),'apply_kwargs',(lambda f,d: f(**d))) or 0")
;Add to builtins a function that can access by index:
!(py-eval "setattr(__import__('builtins'),'index', (lambda o,i: o[i])) or 0")

;Function to prompt GPT via OpenAI API:
(= (useGPT $prompt)
   (let* (($client    (py-call (openai.OpenAI)))
          ($responses (py-call (getattr $client "responses")))
          ($create    (py-call (getattr $responses "create")))
          ($mydict    (py-str  ("{'model':'gpt-5','input':\"" $prompt "\"}")))
          ($kwargs    (once (py-eval $mydict)))
          ($res       (py-call (builtins.apply_kwargs $create $kwargs)))
          ($output    (py-call (getattr $res "output")))
          ($outmsg    (py-call (builtins.index $output 1))) ;0 is ResponseReasoningItem
          ($content   (py-call (getattr $outmsg "content")))
          ($part      (py-call (builtins.index $content 0)))
          ($text      (py-call (getattr $part "text"))))
         $text))
