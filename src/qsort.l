{"完整的快速排序實現，包含全部依賴關係"}

{"LISP算子"}
(define car  (lambda [L] (get L 1)))
(define cdr  (lambda [L] (remove (copy L) 1)))
(define cons (lambda [e L] (insert (copy L) 1 e)))

{"擴展算子 (extend [1 2] [3 4]) -> [1 2 3 4]"}
(define extend (lambda [x y] (cond
    [(eq x []) y]
    [#true (cons (car x) (#lambda (cdr x) y))]
)))

{"規約算子 (reduce (lambda [a b] (mul a b)) [1 2 3 4 5]) -> 5"}
(define reduce (lambda [Fn L] (cond
    [(not-eq L []) ((lambda [n Fn L] (cond
        [(eq L []) n]
        [#true (#lambda (Fn n (car L)) Fn (cdr L))]
    )) (car L) Fn (cdr L))]
)))

{"合併算子 (merge [1 2] [3 4] [5 6] [7 8]) -> [1 2 3 4 5 6 7 8]"}
(define merge (lambda [] (reduce extend #args)))

{"過濾算子 只在Fn返回 #true 時保留元素 (filter (lambda [x] (> x 1800)) [322 629 753 1225 1707 1818 1939 3383 3639 3875]) ->
 [1818 1939 3383 3639 3875]"}
(define filter (lambda [Fn L] (cond
    [(eq L []) []]
    [(eq (Fn (car L)) #true) (cons (car L) (#lambda Fn (cdr L)))]
    [#true (#lambda Fn (cdr L))]
)))

{"比較算子"}
(define ge (lambda [a b] (not (lt a b))))
(define le (lambda [a b] (not (gt a b))))
(define not-eq (lambda [a b] (not (eq a b))))
(define < lt)
(define > gt)
(define >= ge)
(define <= le)

{"自增算子"}
(define inc (lambda [x] (add x 1)))

{"通用快速排序實現，Fn作用于L中的元素，並返回用於排序的值
 對於 (qsort0 [[2 23] [1 43] [5 23] [4 21] [3 54]] (lambda [E] (car E)))
 取其中一項為[2 23]
 求((lambda [E] (car E)) [2 23]) -> 2
 依此類推
 最終結果為 [[1 43] [2 23] [3 54] [4 21] [5 23]]"}
(define qsort0 (lambda [L Fn] (cond
    [(<= (length L) 1) L]
    [#true (merge
        (#lambda (filter (lambda [i] (< (Fn i) (Fn (car L)))) (cdr L)) Fn)
        [(car L)]
        (#lambda (filter (lambda [i] (>= (Fn i) (Fn (car L)))) (cdr L)) Fn)
    )]
)))

{"對於數列的快速排序, 對於[2 3 4 ...]，取其中元素2，帶入常函數(const 2)仍得2，以此排序
 (qsort-number [1818 3875 1225 753 3383 1707 3639 1939 322 629]) ->
    [322 629 753 1225 1707 1818 1939 3383 3639 3875]"}
(define const (lambda [n] n))
(define qsort (lambda [L] (qsort0 L const)))

(define L [1818 3875 1225 753 3383 1707 3639 1939 322 629])
(output "(qsort)\n[1818 3875 1225 753 3383 1707 3639 1939 322 629] -->\n")
(output (qsort L))


