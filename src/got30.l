{"從 1 到 15 間隔 2 以求和，問有無得 30 者。"}
(import "lib.l")

(define L (range 1 15 2))

(map 
    (lambda [a] 
    (map 
        (lambda [b] 
        (map 
            (lambda [c] 
                (println a '+' b '+' c '=' (define n (+ a b c)) (if (eq n 30) '*' ''))
            ) 
        L))
    L)) 
L)
