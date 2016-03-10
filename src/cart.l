(import 'lib.l')

(define cart-create (lambda [] (dict 
    ['prices' (dict 
        ['butter' 1.00] 
        ['milk'   3.00] 
        ['eggs'   6.95]
    )]
    
    ['products' (dict)]
)))

(define cart-add (lambda [cart product-name quantity] (dict-set (dict-get cart 'products') product-name quantity)))

(define cart-get-total (lambda [cart tax-rate] (progn
    (define total-price 0.0)
    (define tax-factor (+ 1.0 tax-rate))
    
    (dict-walk (lambda [product-name quantity] (update total-price 
        (+  total-price (* quantity (dict-get (dict-get cart 'prices') product-name) tax-factor))
    )) (dict-get cart 'products'))
    
    (round total-price 2)
)))

(define my-cart (cart-create))

(cart-add my-cart 'butter' 1)
(cart-add my-cart 'milk' 3)
(cart-add my-cart 'eggs' 6)

(println (cart-get-total my-cart 0.05))