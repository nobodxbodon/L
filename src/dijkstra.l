
(import "lib.l")

(define V ['N0' 'N1' 'N2' 'N3' 'N4'])
(define E [['N0' 'N1' 100] ['N0' 'N2' 30] ['N0' 'N4' 10] ['N2' 'N1' 60] ['N2' 'N3' 60] ['N3' 'N1' 10] ['N4' 'N3' 50]])
(define G [V E])

(define w (lambda [E] (caddr E)))

(define dijkstra (lambda [G w s] (progn
    (define D [])
    (define D-add (lambda [x] (set D (cons x D))))
    (define D-set (lambda [v n] (set D ((lambda [L] (cond
        [(empty-p L) []]
        [(eq (caar L) v) (cons [v n] (cdr L))]
        [#true (cons (car L) (#lambda (cdr L)))]
    )) D))))
    (define D-get (lambda [v] ((lambda [L] (cond
        [(empty-p L) #none]
        [(eq (caar L) v) (cadar L)]
        [#true (#lambda (cdr L))]
    )) D)))
    
    ((lambda [L] (cond
        [(not-eq L []) (progn
            (D-add [(car L) -1])
            (#lambda (cdr L))
        )]
    )) (car G))
    
    ((lambda [L] (cond
        [(empty-p L) #none]
        [(eq (caar L) s) (D-set (cadar L) (w (car L)))]
        [#true (#lambda (cdr L))]
    )) (cadr G))
    
    (D-set s 0) 
    
    (define S [])
    (define Q (exclude s (car G)))
    
    ((lambda [] (cond
        [(not-eq Q []) (progn
            (define u ((lambda [L _min_] (cond
                [(empty-p L) _min_]
                [(< (D-get (car L)) (D-get _min_)) (#lambda (cdr L) (car L))]
                [#true (#lambda (cdr L) _min_)]
            )) (cdr Q) (car Q)))
            
            (set S (cons u S))
            (set Q (exclude u Q))
            
            ((lambda [L] (if (not-eq L []) (progn
                (if 
                    (and 
                        (eq (caar L) u)
                
                        ((lambda [a b] (cond
                            [(and (eq a -1) (not-eq b -1)) #true]
                            [(and (not-eq a -1) (eq b -1)) #false]
                            [#true (> a b)]
                        )) (D-get (cadar L)) ((lambda [a b] (cond
                            [(eq a -1) -1]
                            [#true (add a b)]
                        )) (D-get u) (w (car L))))
                    )
                    
                    (D-set (cadar L) (+ (D-get u) (w (car L))))
                )
                (#lambda (cdr L))
            ))) (cadr G))
            
            (#lambda)
        )]
    )))
    D
)))

((define main (lambda [] ((lambda [L] (cond
    [(not-eq L []) (progn
        (display (string (car L)) (sep) (dijkstra G w (car L)))
        (#lambda (cdr L))
    )]
)) V))))