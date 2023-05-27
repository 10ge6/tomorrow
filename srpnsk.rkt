#lang racket
(require 2htdp/image)

(let sierpinski ([n 8])
  (if (zero? n)
      (triangle 2 'solid 'blue)
      (let ([t (sierpinski (- n 1))])
        (freeze (above t (beside t t))))))

(text "racket" 36 "red")


(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

(define (not-atom? x)
  (not (atom? x)))

(define (lat? L)
  (cond
    [(null? L) #t]
    [(not-atom? (first L)) #f]
    [else (lat? (rest L))]
  )
)

(define (member? a L)
  (cond
    [(null? L) #f]
    [(eqv? a (first L)) L]
    [else (member? a (rest L))]
  )
)


(define (sum L)
  (if (null? L) 0 (+ (first L) (sum (rest L))))
)

(define (mul L)
  (if (null? L) 1 (* (first L) (mul (rest L))))
)

(define (tail_recursive_sum_aux L a)
  (if (null? L) a (tail_recursive_sum_aux (rest L) (+ (first L) a))))

(define (tail_recursive_sum L)
  (tail_recursive_sum_aux L 0)
)

(define (tail_recursive_mul_aux L a)
  (if (null? L) a (tail_recursive_mul_aux (rest L) (* (first L) a))))

(define (tail_recursive_mul L)
  (tail_recursive_mul_aux L 1)
)


(define (invert L)
  (invert_aux L '())
)

(define (invert_aux L a)
  (if (null? L) a (invert_aux (rest L) (cons (first L) a)))
)


(define (remove x L)
  (cond
    [(null? L) '()]
    [(eqv? (first L) x) (rest L)]
    [else (cons (first L) (remove x (rest L)))]
  )
)

(define (remove_all x L)
  (cond
    [(null? L) '()]
    [(eqv? (first L) x) (remove_all x (rest L))]
    [else (cons (first L) (remove_all x (rest L)))]
  )
)
