(import "lib.l")

(define rust-homepage (lambda [] (let
    [
        [program "++*-+*-/-+*-/++*-+-+*-/"]
    ]
    
    (display program "->" ((lambda [L n] (cond
        [(eq L []) n]
        [(eq (car L) '+') (#lambda (cdr L) (add n 1))]
        [(eq (car L) '-') (#lambda (cdr L) (sub n 1))]
        [(eq (car L) '*') (#lambda (cdr L) (mul n 2))]
        [(eq (car L) '/') (#lambda (cdr L) (div n 2))]
        [#true (#lambda (cdr L) n)]
    )) (split program) 0))
)))

(define rust-helloworld (lambda [] (display "Hello World!")))

(define main rust-homepage)

(main)