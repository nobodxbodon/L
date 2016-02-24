
(import "lib.l")

(define point (lambda [x y] [x y]))
(define point? (lambda [P] (eq (type-type P) [:number :number])))
(define point-set-x (lambda [P n] (set P 1 n)))
(define point-get-x (lambda [P] (get P 1)))
(define point-set-y (lambda [P n] (set P 2 n)))
(define point-get-y (lambda [P] (get P 2)))

((lambda [] (progn
    (define P (point 2 3))
    (display (point-get-x P) (point-get-y P)) (newline)
    (point-set-x P 1)
    (point-set-y P 1)
    (display (point-get-x P) (point-get-y P)) (newline)
    (display (point? P)) (newline)
)))