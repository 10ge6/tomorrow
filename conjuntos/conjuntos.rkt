#lang racket
(provide (all-defined-out))  ; exports all functions
(require math/base)
; (require 2htdp/image)
; (require 'nest)


#|
(overlay (let sierpinski ([n 7])
           (if (zero? n)
               (isosceles-triangle 7 300 'solid (make-color (random 256) (random 256) (random 256)))
               (let ([t (sierpinski (- n 1))])
                 (freeze (above (beside t t) t)))))
         (regular-polygon 20 5 "solid" (make-color  50  50 255))
         (regular-polygon 26 5 "solid" (make-color 100 100 255))
         (regular-polygon 32 5 "solid" (make-color 150 150 255))
         (regular-polygon 38 5 "solid" (make-color 200 200 255))
         (regular-polygon 44 5 "solid" (make-color 250 250 255)))

(text "DrRacket" 36 "yellowgreen")
|#

; atom? list? -> list?
(define (remove_all* a GL)
  (cond
    [(null? GL) null]
    [(list? (first GL))
     (cons (remove_all* a (first GL)) (remove_all* a (rest GL)))
     ]
    [(eqv? a (first GL)) (remove_all* a (rest GL))]
    [else (cons (first GL) (remove_all* a (rest GL)))]
    )
  )


(define (distancia x y)
  (sqrt (+ (sqr x) (sqr y)))
  )

(define (maior-str? str1 str2)
  (let ([L1 (string->list str1)]
        [L2 (string->list str2)])
    (comp-listas-char? L1 L2)
    )
  )

(define (comp-listas-char? L1 L2)
  (cond
    [(and (null? L1) (null? L2)) #f]
    [(null? L2) #t]
    [(null? L1) #f]
    [(char>? (char-upcase (first L1)) (char-upcase (first L2))) #t]
    [(eq? (char-upcase (first L1)) (char-upcase (first L2)))
     (comp-listas-char? (rest L1) (rest L2))]
    [else #f]
    )
  )

(define (maior-str2? str1 str2)
  (string-ci>? str1 str2)
  )

(module nest racket
  (provide num-eggs)
  (define num-eggs 2))

(define (rem* L1 L2)
  (if (null? L1)
      L2
      (rem* (rest L1)
            (remove_all* (first L1) L2))
      ))

(define (my-map f L)
  (if (null? L)
      L
      (cons (f (first L)) (my-map f (rest L)))
      )
  )

; tricky
(define (my-variadic-map f . LL)
  (if (null? (first LL))
      null
      (cons (apply f (my-map first LL))
            (apply my-variadic-map
                   f
                   (my-map rest LL)))
      )
  )

(define (my-foldl f init L)
  (if (null? L)
      init
      (my-foldl f
                (f (first L) init)
                (rest L))
      )
  )

; trickier
(define (my-variadic-foldl f init . LL)
  (if (null? (first LL))
      init
      (apply my-variadic-foldl
             f
             (apply f (append (my-map first LL)
                              (list init)))
             (my-map rest LL))
      )
  )

(define (divide-lista L [L1 `()] [L2 `()])
  (if (null? L)
      (values L1 L2)
      (divide-lista (rest L)
                    L2
                    (cons (first L) L1))
      )
  )

(define (merge L1 L2)
  (cond
    [(null? L1) L2]
    [(null? L2) L1]
    [(<= (first L1) (first L2))
     (cons (first L1)
           (merge (rest L1) L2))]
    [else
     (cons (first L2)
           (merge (rest L2) L1))]
    )
  )

(define (mergesort L)
  (cond
    [(null? L) '()]
    [(null? (rest L)) L]
    [else
     (let-values ([(L1 L2) (divide-lista L)])
       (merge (mergesort L1) (mergesort L2))
       )]
    )
  )

(define (merge-2 L1 L2 #:func [f <=] #:key [k identity])
  (cond
    [(null? L1) L2]
    [(null? L2) L1]
    [(f (k (first L1)) (k (first L2)))
     (cons (first L1)
           (merge-2 (rest L1) L2 #:func f #:key k))]
    [else
     (cons (first L2)
           (merge-2 (rest L2) L1 #:func f #:key k))]
    )
  )

#|
supports stupid shit e.g. calling
(mergesort-2 `((8 3) (5 7 9) (7 6 4 1))
               #:func string<=? #:key (lambda (x)
                                     (let ([y (second x)])
                                     (if (symbol? y)
                                         (symbol->string y)
                                     (number->string y))))
               )
|#
(define (mergesort-2 L #:func [f <=] #:key [k identity])
  (cond
    [(null? L) '()]
    [(null? (rest L)) L]
    [else
     (let-values ([(L1 L2) (divide-lista L)])
       (merge-2
        (mergesort-2 L1 #:func f #:key k)
        (mergesort-2 L2 #:func f #:key k)
        #:func f #:key k)
       )]
    )
  )

#|
(define (divide-lista 2 L [flg #t])
  (cond
    [(null? L) (values `() `())]
    [flg
     (let-values
         ([(L1 L2) (divide-lista2 (rest L) #f)])
       (values (cons (first L) L1)
               L2))]
    [else
     (let-values)] ...
    )
  )
|#

(define (remove-tudo-v3 e l #:fc [f eqv?] #:cc [c identity])
  (cond
    [(null? l) l]
    [(f e (c (first l)))
     (remove-tudo-v3 e (rest l) #:fc f #:cc c)]
    [else
     (cons (first l)
           (remove-tudo-v3 e (rest l) #:fc f #:cc c))]
    )
  )

(define (rem-enc e l #:fc [f eq?] #:cc [c identity])
  (let [(dado (remove-tudo-v3 e l #:fc f #:cc c))]
    (lambda(p)
      (cond
        [(eq? p `r) dado]
        [(eq? p `l) (length dado)]
        [(exact-nonnegative-integer? p) (list-ref dado p)]
        ))
    )
  )

(define (cria-oper o) ; or (define cria-oper \n (lambda (o) \n (lambda (x y) ...
  (lambda(x y)
    (cond
      [(eq? o `somatorio) (foldl + 0 (range x (+ y 1)))] ; or (foldl + x (range (+ x 1) (+ y 1)))
      [(eq? o `soma) (+ x y)]
      [(eq? o `mul) (* x y)]
      ))
  )
