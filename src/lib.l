(define get (let [[get get] [eq eq]] (lambda [seq i] 
    (if (eq (type seq) :string) 
        (decode [(get (encode seq) i)]) 
        (get seq i)
    )
)))

(define set (let [[set set] [eq eq]] (lambda [seq i o]
    (if (eq (type seq) :string)
        (decode (set (encode seq) i (car (encode o))))
        (set seq i o)
    )
)))

(define insert (let [[insert insert] [eq eq]] (lambda [seq i o]
    (if (eq (type seq) :string) 
        (decode (insert (encode seq) i (car (encode o))))
        (insert seq i o)
    )
)))

(define delete (let [[delete delete] [eq eq]] (lambda [seq i] 
    (if (eq (type seq) :string) 
        (decode (delete (encode seq) i))
        (delete seq i)
    )
)))

(define length (let [[length length] [eq eq]] (lambda [seq]
    (if (eq (type seq) :string)
        (length (encode seq))
        (length seq)
    )
)))

(define copy (let [[copy copy] [eq eq]] (lambda [seq]
    (if (eq (type seq) :string) 
        seq
        (copy seq)
    )
)))

(define car  (lambda [L]   (get L 1)))
(define cdr  (lambda [L]   (delete (copy L) 1)))
(define cons (lambda [e L] (insert (copy L) 1 e)))

(define kar  (lambda [L]   (get L -1)))
(define kdr  (lambda [L]   (delete (copy L) -1)))
(define kons (lambda [L e] (insert (copy L) -1 e)))

(define caar  (lambda [L] (car (car L))))
(define cadar (lambda [L] (car (cdr (car L)))))
(define cadr  (lambda [L] (car (cdr L))))
(define cddr  (lambda [L] (cdr (cdr L))))
(define caddr (lambda [L] (car (cdr (cdr L)))))

(define ++ (define inc (lambda [x] (add x 1))))
(define -- (define dec (lambda [x] (sub x 1))))

(define + (lambda [] (reduce add #args)))
(define - (lambda [] (reduce sub #args)))
(define * (lambda [] (reduce mul #args)))
(define / (lambda [] (reduce div #args)))
(define % (define mod (lambda [a b] (sub a (mul (trunc (div a b)) b)))))
(define ^ (define exp (lambda [a b] ((lambda [x y] (cond
    [(eq b 0) 1]
    [(eq x b) y]
    [#true (#lambda (add x 1) (mul a y))]
)) 1 a))))
(define abs (lambda [] (if (< x 0) (sub 0 x) x)))

(define default (lambda [value default_value] (if (eq value #none) default_value value)))

(define and (lambda [] (reduce (lambda [a b] (if a b #false)) #args)))
(define or  (lambda [] (reduce (lambda [a b] (if a #true b)) #args)))
(define not (lambda [a] (if a #false #true)

(define true  (lambda [] #true))
(define false (lambda [] #false))
(define const (lambda [n] n))

(define reverse (lambda [L] (cond
    [(empty? L) L]
    [#true (cons (kar L) (#lambda (kdr L)))]
)))

(define multi_relational_op (let [[eq eq]] (lambda [op] (lambda []
    ((lambda [L] (cond
        [(eq (length L) 2) (op (car L) (cadr L))]
        [(op (car L) (cadr L)) (#lambda (cdr L))]
        [#true #false]
    )) #args)
))))

(define >  (define gt (multi_relational_op gt)))
(define >= (define ge (multi_relational_op (lambda [a b] (not (lt a b))))))
(define <  (define lt (multi_relational_op lt)))
(define <= (define le (multi_relational_op (lambda [a b] (not (gt a b))))))

(define =  (define eq (multi_relational_op eq)))
(define != (define not-eq (define ne (lambda [] (not (apply = #args))))))

(define $ (lambda [Fn] (lambda [] ((kar #args) (apply Fn (kdr #args))))))

(define map (lambda [Fn L] (cond
    [(empty? L) []]
    [#true (cons (Fn (car L)) (#lambda Fn (cdr L)))]
)))                                                        

(define reduce (lambda [Fn L] 
    ((lambda [n Fn L] (cond
        [(empty? L) n]
        [#true (#lambda (Fn n (car L)) Fn (cdr L))]
    )) (car L) Fn (cdr L))
))

(define trav (lambda [Fn L] (cond
    [(empty? L) []]
    [(list? (car L)) (cons (#lambda Fn (car L)) (#lambda Fn (cdr L)))]
    [#true (cons (Fn (car L)) (#lambda Fn (cdr L)))]
)))

(define filter (lambda [Fn L] (cond
    [(empty? L) []]
    [(eq (Fn (car L)) #true) (cons (car L) (#lambda Fn (cdr L)))]
    [#true (#lambda Fn (cdr L))]
)))

(define factorial  (lambda [n] (if (eq n 0) 1 (* n (#lambda (- n 1))))))
(define factorial0 (lambda [n] (reduce * (range n))))
(define factorial1 (lambda [n] (apply * (range n))))
(define factorial$ (lambda [n k] (cond
    [(eq n 0) (k 1)]
    [#true (#lambda (dec n) (lambda [v] (k (* n v))))]
)))

(define fibonacci (lambda [n] 
    ((lambda [a b i] (cond
        [(eq n i) []]
        [#true (cons a (#lambda b (add a b) (add i 1)))]
    )) 0 1 0)
))

(define length-eq (lambda [L n] (eq (length L) n)))

(define odd?        (lambda [x] (not (even? x))))
(define even?       (lambda [x] (eq (% x 2) 0)))
(define negative?   (lambda [x] (< x 0)))
(define postive?    (lambda [x] (>= x 0)))

(define list?   (lambda [e] (eq (type e) :list)))
(define atom?   (lambda [e] (not (list? e))))
(define empty?  (lambda [L] (eq (length L) 0)))
(define number? (lambda [v] (eq (type v) :number)))
(define string? (lambda [v] (eq (type v) :string)))
(define bool?   (lambda [v] (eq (type v) :bool)))
(define none?   (lambda [v] (eq (type v) :none)))
(define error?  (lambda [v] (eq (type v) :error)))
(define lambda? (lambda [v] (eq (type v) :lambda)))

(define in (lambda [e L] (cond
    [(empty? L) #false]
    [(eq (car L) e) #true]
    [#true (#lambda e (cdr L))]
)))

(define not-in (lambda [e L] (not (in e L))))

(define slice (lambda [seq start end] (cond
    [(> start end) (if (string? seq) "" [])]
    [#true (cons (get L start) (#lambda seq (inc start) end))]
)))

(define exclude (lambda [e L] 
    ((lambda [L] (cond
        [(empty? L) []]
        [(eq (car L) e) (#lambda (cdr L))]
        [#true (cons (car L) (#lambda (cdr L)))]
    )) L)
))

(define merge (lambda [] (reduce (lambda [L1 L2] (cond
    [(empty? L1) L2]
    [#true (cons (car L1) (#lambda (cdr L1) L2))]
)) #args)))

(define zip (lambda [] (reduce (lambda [L1 L2] (cond
    [(empty? L2) []]
    [#true (cons (kons (car L1) (car L2)) (#lambda (cdr L1) (cdr L2)))]
)) (set #args 1 ((lambda [L] (if (empty? L) [] (cons [(car L)] (#lambda (cdr L))))) (car #args))))))

(define mix (lambda [L] (cond
    [(empty? L) []]
    [(list? (car L)) (merge (#lambda (car L)) (#lambda (cdr L)))]
    [#true (cons (car L) (#lambda (cdr L)))]
)))

(define dict (lambda [] #args))

(define dict-get (lambda [L key] ((lambda [L] (cond
    [(empty? L) #none]
    [(eq key (caar L)) (get (car L) 2)]
    [#true (#lambda (cdr L))]
)) L)))

(define dict-update (lambda [L key value] ((lambda [L] (cond
    [(empty? L) #none]
    [(eq key (caar L)) (set (car L) 2 value)]
    [#true (#lambda (cdr L))]
)) L)))

(define dict-set (lambda [D name value] (if (eq (dict-update D name value) #none) (insert D -1 [name value]))))

(define dict-walk (lambda [Fn D] 
    (map (lambda [item] (Fn (car item) (cadr item))) D)
))

(define dict-keys (lambda [D] (cond
    [(empty? D) []]
    [#true (cons (caar D) (#lambda (cdr D)))]
)))

(define dict-values (lambda [D] (cond
    [(empty? D) []]
    [#true (cons (cadar D) (#lambda (cdr D)))]
)))
(define dict-items (lambda [D] D))

(define dict-delete (lambda [D k] 
    ((lambda [n] (cond
        [(> n (dict-length D)) #false]
        [(eq (car (get D n)) k) (progn (delete D n) D)]
        [#true (#lambda (inc n))]
    )) 1)
))


(define dict-length (lambda [D] (length D)))

(define dict? (lambda [L] (all (map (lambda [e] (and (list? e) (length-eq e 2))) L))))

(define qsort0 (lambda [L Fn] (cond
    [(<= (length L) 1) L]
    [#true (merge
        (#lambda (filter (lambda [i] (< (Fn i) (Fn (car L)))) (cdr L)) Fn)
        [(car L)]
        (#lambda (filter (lambda [i] (>= (Fn i) (Fn (car L)))) (cdr L)) Fn)
    )]
)))

(define qsort (lambda [L] (qsort0 L const)))

(define split (lambda [s] (map (lambda [c] (decode [c])) (encode s))))

(define max (lambda [L] 
    ((lambda [n L] (cond
        [(empty? L) n]
        [(> (car L) n) (#lambda (car L) (cdr L))]
        [#true (#lambda n (cdr L))]
    )) (car L) (cdr L))
))

(define min (lambda [L] 
    ((lambda [n L] (cond
        [(empty? L) n]
        [(< (car L) n) (#lambda (car L) (cdr L))]
        [#true (#lambda n (cdr L))]
    )) (car L) (cdr L))
))


(define dotimes (lambda [n Fn] ((lambda [x] (cond [(<= x n) (progn (Fn) (#lambda (inc x)))])) 1)))
(define forever (lambda [Fn] (progn (Fn) (#lambda))))

(define str-map (lambda [Fn s] (map (lambda [] (Fn (decode #args))) (encode s))))

(define dup (lambda [v n] (cond
    [(eq n 0) []]
    [#true (cons v (#lambda v (dec n)))]
)))

(define input (let [[input input]] (lambda [s] (progn (output s) (input)))))

(define output (let [[output output]] (lambda [] 
    ((lambda [L] (cond
        [(empty? L) #none]
        [#true (progn (output (car L)) (#lambda (cdr L)))]
    )) #args)
)))

(define println (lambda [] (progn (apply output #args) (newline))))

(define newline (lambda [] (output "\000A")))

(define LF  "\000A")
(define BEL "\0007")

(define at (lambda [e L] ((lambda [e L n] (cond
    [(empty? L) #none]
    [(eq (car L) e) n]
    [#true (#lambda e (cdr L) (inc n))]
)) e L 1)))

(define unique (lambda [L] ((lambda [L C] (cond
    [(empty? L) C]
    [(not-in (car L) C) (#lambda (cdr L) (kons C (car L)))]
    [#true (#lambda (cdr L) C)]
)) L [])))

(define bool (lambda [x] (cond
    [(or (eq x 0) (eq x #none) (eq x #false) (eq x []) (eq x "")) #false]
    [#true #true]
)))

(define range (lambda [] ((lambda [s e i] (cond
    [(> s e) []]
    [#true (cons s (#lambda (+ s i) e i))]
)) (get #args 1) (get #args 2) (if (length-eq #args 3) (get #args 3) 1))))
