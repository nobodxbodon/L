{"lib.l"}

{"LISP"}
(define car  (lambda [L] (get L 1)))
(define cdr  (lambda [L] (remove (copy L) 1)))
(define cons (lambda [e L] (insert (copy L) 1 e)))
(define atom (lambda [A] (not (list? A))))

(define caar  (lambda [L] (car (car L))))
(define cadar (lambda [L] (car (cdr (car L)))))
(define cadr  (lambda [L] (car (cdr L))))
(define cddr  (lambda [L] (cdr (cdr L))))
(define caddr (lambda [L] (car (cdr (cdr L)))))

{"factorial"}
(define factorial (lambda [n] (cond [(eq n 0) 1] [#true (* n (#lambda (- n 1)))])))
(define factorial0 (lambda [n] (reduce * (range n))))
(define factorial1 (lambda [n] (apply * (range n))))
(define factorial& (lambda [n k] (cond
    [(eq n 0) (k 1)]
    [#true (#lambda (dec n) (lambda [v] (k (* n v))))]
)))

{"fibonacci"}
(define fibonacci (lambda [n] (reverse ((lambda [n L] (cond
    [(eq n 1) (cdr L)]
    [#true (#lambda (dec n) (cons (add (car L) (cadr L)) L))]
)) n [1 0]))))

{"cmp-number"}
(define = eq)
(define < lt)
(define > gt)
(define >= (lambda [a b] (not (lt a b))))
(define <= (lambda [a b] (not (gt a b))))
(define != (lambda [a b] (not (eq a b))))
(define not-eq !=)

{"cmp-eq"}
(define length-eq (lambda [L n] (eq (length L) n)))

(define all-eq (lambda [] (progn
    (assert (>= (length #args) 2) "(all-eq): 最少需要 2 個或多個參數")
    ((lambda [L] (cond
        [(eq (length L) 2) (eq (car L) (cadr L))]
        [(eq (car L) (cadr L)) (#lambda (cdr L))]
        [#true #false]
    )) #args)
)))

{"?"}
(define list? (lambda [e] (eq (type e) :list)))
(define atom? (lambda [e] (not (list? e))))
(define empty? (lambda [L] (eq L [])))

(define odd? (lambda [x] (not (even-p x))))
(define even? (lambda [x] (eq (mod x 2) 0)))

(define negative? (lambda [x] (< x 0)))
(define postive? (lambda [x] (>= x 0)))

(define number? (lambda [v] (eq (type v) :number)))
(define bool? (lambda [v] (eq (type v) :boolean)))
(define none? (lambda [v] (eq (type v) :none)))
(define error? (lambda [v] (eq (type v) :error)))
(define lambda? (lambda [v] (eq (type v) :lambda)))

{"list"}
(define rcar (lambda [L] (cond
    [(eq (cdr L) []) (car L)]
    [#true (#lambda (cdr L))]
)))

(define rcdr (lambda [L] (cond
    [(eq (cdr L) []) []]
    [#true (cons (car L) (#lambda (cdr L)))]
)))

(define reverse (lambda [L] (cond
    [(eq L []) L]
    [#true (cons (rcar L) (#lambda (rcdr L)))]
)))

(define map (lambda [Fn L] (cond
    [(empty? L) []]
    [#true (cons (Fn (car L)) (#lambda Fn (cdr L)))]
)))

(define foreach (lambda [Fn L] (cond
    [(empty? L) []]
    [(list? (car L)) (cons (#lambda Fn (car L)) (#lambda Fn (cdr L)))]
    [#true (cons (Fn (car L)) (#lambda Fn (cdr L)))]
)))

(define reduce (lambda [Fn L] (cond
    [(not-eq L []) ((lambda [n Fn L] (cond
        [(eq L []) n]
        [#true (#lambda (Fn n (car L)) Fn (cdr L))]
    )) (car L) Fn (cdr L))]
)))

(define filter (lambda [Fn L] (cond
    [(eq L []) []]
    [(eq (Fn (car L)) #true) (cons (car L) (#lambda Fn (cdr L)))]
    [#true (#lambda Fn (cdr L))]
)))

(define in (lambda [e L] (cond
    [(eq L []) #false]
    [(eq (car L) e) #true]
    [#true (#lambda e (cdr L))]
)))

(define not-in (lambda [e L] (not (in e L))))

(define slice (lambda [L start end] (progn
    {(cond
        [(eq (length #args) 2) (define end -1)]
        [(eq (length #args) 3) (define end (rcar #args))]
        [#true (error 0)]
    )}
    (if (negative-p start) (set start (+ (length L) start 1)))
    (if (negative-p end) (set end (+ (length L) end 1)))
    ((lambda [r] (cond
        [(eq r (error 0)) r]
        [#true ((lambda [r] (cond
            [(eq r (error 0)) r]
            [#true r]
        )) (tail r (- (inc end) start)))]
)) (head L end)))))

(define tail (lambda [L n] (cond
    [(or (negative-p n) (> n (length L))) (error 0)]
    [(eq n 0) []]
    [#true ((lambda [L n] (cond
        [(eq n 0) L]
        [#true (#lambda (cdr L) (dec n))]
    )) L (- (length L) n))]
)))

(define head (lambda [L n] (cond
    [(or (negative-p n) (> n (length L))) (error 0)]
    [(eq n 0) []]
    [#true ((lambda [L n] (cond
        [(eq n 0) []]
        [#true (cons (car L) (#lambda (cdr L) (dec n)))]
    )) L n)]
)))
{(mix [2] 1 2 [2] 2) -> [2 1 2 2 2]}
(define mix (lambda [] ((lambda [L L2] (cond
    [(eq L []) L2]
    [#true (if (eq (type (car L)) :list) (#lambda (cdr L) (extend L2 (car L))) (#lambda (cdr L) (append0 L2 (car L))))]
)) #args [])))
(define exclude (lambda [e L] ((lambda [L] (cond
    [(empty-p L) []]
    [(eq (car L) e) (#lambda (cdr L))]
    [#true (cons (car L) (#lambda (cdr L)))]
)) L)))
(define extend (lambda [x y] (cond
    [(eq x []) y]
    [#true (cons (car x) (#lambda (cdr x) y))]
)))


(define merge (lambda [] (reduce extend #args)))

(define zip (lambda [L1 L2] (progn
    (assert (eq (length L1) (length L2)))
    ((lambda [L1 L2] (cond
        [(empty-p L1) []]
        [#true (cons [(car L1) (car L2)] (#lambda (cdr L1) (cdr L2)))]
    )) L1 L2)
)))

(define zip3 (lambda [L1 L2 L3] (progn
    (assert (and (eq (length L1) (length L2)) (eq (length L2) (length L3))))
    ((lambda [L1 L2 L3] (cond
        [(empty-p L1) []]
        [#true (cons [(car L1) (car L2) (car L3)] (#lambda (cdr L1) (cdr L2) (cdr L3)))]
    )) L1 L2 L3)
)))


{"dict"}
(define dict (lambda [] #args))

(define dict-get (lambda [L key] ((lambda [L] (cond
    [(eq L []) #none]
    [(eq key (caar L)) (get (car L) 2)]
    [#true (#lambda (cdr L))]
)) L)))

(define dict-update (lambda [L key value] ((lambda [L] (cond
    [(eq L []) #none]
    [(eq key (caar L)) (set (car L) 2 value)]
    [#true (#lambda (cdr L))]
)) L)))

(define dict-set (lambda [D name value] (if (eq (dict-update D name value) #none) (insert D 1 [name value]))))

(define dict-walk (lambda [Fn D] 
    (map (lambda [item] (Fn (car item) (cadr item))) D)
))

(define dict-keys pass)
(define dict-values pass)

(define dict-del pass)
(define dict-length pass)

(define dict? (lambda [L] (all (foreach (lambda [e] (and (list? e) (length-eq e 2))) L))))

{"type"}


(define type-type (lambda [L] ((lambda [L] (if
    (atom? L)
    (type L)
    ((lambda [L] (cond
        [(empty? L) []]
        [(list? (car L)) (cons (#lambda (car L)) (#lambda (cdr L)))]
        [#true (cons (type (car L)) (#lambda (cdr L)))]
    )) L)
))L)))

{"qsort"}
(define qsort0 (lambda [L Fn] (cond
    [(<= (length L) 1) L]
    [#true (merge
        (#lambda (filter (lambda [i] (< (Fn i) (Fn (car L)))) (cdr L)) Fn)
        [(car L)]
        (#lambda (filter (lambda [i] (>= (Fn i) (Fn (car L)))) (cdr L)) Fn)
    )]
)))
{"(qsort0 [[2 23] [1 43] [5 23] [4 21] [3 54]] (lambda [E] (car E))) ->"}
{"[[1 43] [2 23] [3 54] [4 21] [5 23]]"}

(define qsort (lambda [L] (qsort0 L const)))

{"utils"}
(define ++ (define inc (lambda [x] (add x 1))))
(define -- (define dec (lambda [x] (sub x 1))))

(define + (lambda [] (reduce add #args)))
(define - (lambda [] (reduce sub #args)))
(define * (lambda [] (reduce mul #args)))
(define / div)
(define % mod)
(define ^ exp)

(define log10 (lambda [x] (log x 10)))
(define log2 (lambda [x] (log x 2)))

(define abs (lambda [] (if (< x 0) (sub 0 x) x)))

(define true (lambda [] #true))
(define false (lambda [] #false))
(define const (lambda [n] n))

(define sep (lambda [] " "))

(define max (lambda [L] (cond
    [(not-eq L []) ((lambda [x L] (cond
        [(eq L []) x]
        [(> (car L) x) (#lambda (car L) (cdr L))]
        [#true (#lambda x (cdr L))]
    )) (car L) (cdr L))]
)))

(define min (lambda [L] (cond
    [(not-eq L []) ((lambda [x L] (cond
        [(eq L []) x]
        [(< (car L) x) (#lambda (car L) (cdr L))]
        [#true (#lambda x (cdr L))]
    )) (car L) (cdr L))]
)))
(define all (lambda [L] (cond
    [(eq L []) #true]
    [(not-eq (car L) #true) #false]
    [#true (#lambda (cdr L))]
)))

(define any (lambda [L] (cond
    [(eq L []) #false]
    [(eq (car L) #true) #true]
    [#true (#lambda (cdr L))]
)))
(define Random (lambda [seed] (lambda [] (set seed (% (* seed 10807) (- (^ 2 32) 1))))))

(define random-list (lambda [seed] (progn
    (define rand (Random seed))
    ((lambda [10] (cond
        [(eq 10 0) []]
        [#true (cons (floor (number (/ (rand) 1000000))) (#lambda (dec 10)))]
    )) 10)
)))


(define rep (lambda [v n] (cond
    [(eq n 0) []]
    [#true (cons v (#lambda v (dec n)))]
)))

(define <=> (lambda [a b] (cond
    [(< a b) -1]
    [(= a b)  0]
    [(> a b)  1]
)))


(define display (lambda [] ((lambda [L] (cond
    [(eq (cdr L) []) (progn (output (car L)) (car L))]
    [#true (progn (output (car L)) (output " ") (#lambda (cdr L)))]
)) #args)))

(define println (lambda [] (progn (apply display #args) (newline))))

(define newline (lambda [] (output "\n")))


(define extend (lambda [x y] (cond
    [(eq x []) y]
    [#true (cons (car x) (#lambda (cdr x) y))]
)))

{(mix [2] 1 2 [2] 2) -> [2 1 2 2 2]}
(define mix (lambda [] ((lambda [L L2] (cond
    [(eq L []) L2]
    [#true (if (eq (type (car L)) :list) (#lambda (cdr L) (extend L2 (car L))) (#lambda (cdr L) (append0 L2 (car L))))]
)) #args [])))

(define zip (lambda [L1 L2] (progn
    (assert (eq (length L1) (length L2)))
    ((lambda [L1 L2] (cond
        [(empty-p L1) []]
        [#true (cons [(car L1) (car L2)] (#lambda (cdr L1) (cdr L2)))]
    )) L1 L2)
)))

(define zip3 (lambda [L1 L2 L3] (progn
    (assert (and (eq (length L1) (length L2)) (eq (length L2) (length L3))))
    ((lambda [L1 L2 L3] (cond
        [(empty-p L1) []]
        [#true (cons [(car L1) (car L2) (car L3)] (#lambda (cdr L1) (cdr L2) (cdr L3)))]
    )) L1 L2 L3)
)))
(define all-eq (lambda [] (progn
    (assert (>= (length #args) 2) "(all-eq): 最少需要 2 個或多個參數")
    ((lambda [L] (cond
        [(eq (length L) 2) (eq (car L) (cadr L))]
        [(eq (car L) (cadr L)) (#lambda (cdr L))]
        [#true #false]
    )) #args)
)))
(define pair zip)

(define array (lambda [L] (zip (range (length L)) L)))
(define enumerate array)

(define br (lambda [] "\n"))


(define push (lambda [e L] (insert L 1 e)))
(define pop (lambda [L] (remove L 1)))
(define append (lambda [L e] (insert L (inc (length L)) e)))
(define push0 (lambda [e L] (push e (dup L))))
(define pop0 (lambda [L] (pop (dup L))))
(define append0 (lambda [L e] (append (dup L) e)))

(define insert-into-middle (lambda [e L] (cond
    [(empty-p (cdr L)) (car L)]
    [#true (cons (car L) (cons e (#lambda e (cdr L))))]
)))

(define swift-+ (lambda [a b] (cond
    [(and (eq (type a) :string) (eq (type b) :string)) (concat a b)]
    [(and (eq (type a) :number) (eq (type b) :number)) (add a b)]
    [(or  (eq (type a) :string) (eq (type b) :string)) (concat (string a) (string b))]
    [#true (error 0)]
)))


(define at (lambda [Lorg indexesL] (progn
    (assert (all (map (lambda [e] (and (<= 1 e) (<= e (length Lorg)))) indexesL)) "(at): 索引值有誤")
    ((lambda [L] (cond
        [(empty-p L) []]
        [#true (cons (obtain Lorg (car L)) (#lambda (cdr L)))]
    )) indexesL)
)))



(define compact (lambda [L] (exclude #none L)))

(define uniq (define unique (lambda [L] (progn
    (define _ [])
    ((lambda [L] (cond
        [(empty-p L) _]
        [#true (progn (if (not-in (car L) _) (append _ (car L))) (#lambda (cdr L)))]
    )) L)
))))

{"string"}

(define low-case (lambda [S] (unichar ((lambda [L] (cond
    [(eq L []) []]
    [#true (cons ((lambda [n] (if (and (<= n 90) (>= n 65)) (add n 32) n)) (car L)) (#lambda (cdr L)))]
)) (unicode S)))))

(define up-case (lambda [S] (unichar ((lambda [L] (cond
    [(eq L []) []]
    [#true (cons ((lambda [n] (if (and (<= n 122) (>= n 97)) (sub n 32) n)) (car L)) (#lambda (cdr L)))]
)) (unicode S)))))

(define lower-alphabet (lambda [] ((lambda [L] (cond
    [(empty-p L) []]
    [#true (cons (unichar [(car L)]) (#lambda (cdr L)))]
)) (unicode "abcdefghijklmnopqrstuvwxyz"))))

(define upper-alphabet (lambda [] ((lambda [L] (cond
    [(empty-p L) []]
    [#true (cons (unichar [(car L)]) (#lambda (cdr L)))]
)) (unicode (up-case "abcdefghijklmnopqrstuvwxyz")))))

(define alphabet upper-alphabet)

(define reverse-str (lambda [S] (unichar (reverse (unicode S)))))

(define concat (lambda [] (unichar ((lambda [L] (cond
    [(empty-p L) []]
    [(eq (type (car L)) :string) (extend (unicode (car L)) (#lambda (cdr L)))]
    [#true (extend (unicode (string (car L))) (#lambda (cdr L)))]
)) #args))))


{"bool"}
(define xor (lambda [a b] (cond
    [(or (not-eq (type a) :boolean) (not-eq (type b) :boolean)) (error 0)]
    [#true (cond
        [(eq a b) #false]
        [#true #true]
    )]
)))
(define boolean (lambda [x] (cond
    [(or (eq x 0) (eq x #none) (eq x #false) (eq x []) (eq x "")) #false]
    [#true #true]
)))



(define range (lambda [] (cond
    [(not-eq (cond
        [(eq (length #args) 1) (progn
            (define start  1)
            (define stop   (car #args))
            (define step   1)
        )]

        [(eq (length #args) 2) (progn
            (define start  (car #args))
            (define stop   (car (cdr #args)))
            (define step   1)
        )]

        [(eq (length #args) 3) (progn
            (define start  (car #args))
            (define stop   (car (cdr #args)))
            (define step   (rcar #args))
        )]
    ) #none)

    ((lambda [__start __stop __step] (cond
        [(>= __start __stop) []]
        [(< __start __stop) (cons __start (#lambda (+ __start __step) __stop __step))]
    )) start (inc stop) step)]
)))
