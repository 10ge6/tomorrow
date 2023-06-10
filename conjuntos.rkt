#lang racket
(provide (all-defined-out))  ;exports all functions
(require 2htdp/image)
;(require 'nest)



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
