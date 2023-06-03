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
; or (not (or (pair? a) (null? a)))

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

; ????????????
; (provide (contract-out [rmember (or/c (rmember) (and/c list? (not/c empty?)))]))

(define (dobra L)
  (cond
    [(null? L) null] ; (eqv? null '()) = #t
    [else (cons (* 2 (first L)) (dobra (rest L)))]
    )
  )

(define (rmember x L)
  (cond
    [(null? L) '()]
    [(eqv? (first L) x) (rest L)]
    [else (cons (first L) (rmember x (rest L)))]
    )
  )

(define (remove_all x L)
  (cond
    [(null? L) '()]
    [(eqv? (first L) x) (remove_all x (rest L))]
    [else (cons (first L) (remove_all x (rest L)))]
    )
  )

(define (swap_all x y L)
  (cond
    [(null? L) null]
    [(eqv? (first L) x) (cons y (swap_all x y (rest L)))] #| originally tried (swap_all x y (cons y (rest L)))
                                                             (works but always rets #f going to else (BAD)) |#
    [else (cons (first L) (swap_all x y (rest L)))]
    )
  )

(define (membro? a L)
  (cond
    [(null? L) #f]
    [(eqv? a (first L) #t)]
    [else (membro? a (rest L))]
    )
  )

; GL = generic list
(define (member*? a GL) ; DFS
  (cond
    [(null? GL) #f]
    [(list? (first GL))
     (or
      (member*? a (first GL))
      (member*? a (rest GL)))
     ]
    [(eqv? a (first GL)) #t]
    [else (member*? a (rest GL))]
    )
  )

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

; RL = resulting list
; atom? list? -> list?
(define (remove_once* a GL)
  (let-values ([(b RL) ; b for bool
                (remove_once_iter* a GL)]) RL)
  )

; atom? list? -> boolean? list?
(define (remove_once_iter* a GL)
  (cond
    [(null? GL) (values #f null)]
    [(list? (first GL))
     (let-values ([(b RL) (remove_once_iter* a (first GL))])
       (if b
           (values #t (cons RL (rest GL))) ; removed element, return list
           (let-values ([(b2 RL2) (remove_once_iter* a (rest GL))]) ; keep looking
             (values b2 (cons RL RL2))))                       ; and return the values from that
       )
     ]
    [(eqv? a (first GL))
     (values #t (rest GL))]
    [else (let-values ([(b2 RL2) (remove_once_iter* a (rest GL))])
            (values b2 (cons (first GL) RL2)))]
    )
  )

(define (insert_right* a an GL)
  (cond
    [(null? GL) null]
    [(list? (first GL))
     (cons (insert_right* a an (first GL))
           (insert_right* a an (rest GL)))
     ]
    [(eqv? a (first GL))
     (cons a (cons an (insert_right* a an (rest GL))))
     ]
    [else (cons (first GL) (insert_right* a an (rest GL)))]
    )
  )

(define (invert* L)
  (invert_aux* L '())
  )

(define (invert_aux* GL a)
  (cond
    [(null? GL) a]
    [(list? (first GL))
     (invert_aux* (rest GL) (cons (invert* (first GL)) a))
     ]
    [else (invert_aux* (rest GL) (cons (first GL) a))]
    )
  )

(define (invert_wparams L [a empty]) ; (eqv? `() empty) = #t
  (if (null? L) a (invert_wparams (rest L) (cons (first L) a)))
  )

; any/c ... -> list?
(define (mklist . elementos)
  elementos)

(define (my_sum . vals)
  (my_sum_aux vals)
  )

(define (my_sum_aux s)
  (if (null? s) 0 (+ (first s) (my_sum_aux (rest s))))
  )

(define (my_good_sum . ls)
  (if (null? ls)
      0
      (+ (first ls) (apply my_good_sum (rest ls)))
      )
  )

; integer? any/c ... -> list?
(define (intercala-m n . args)
  (if (< n 1)
      null
      (cons (first args)
            (apply intercala-m
                   (cons (- n 1)
                         (insere-final (first args) (rest args)))))))

(define (insere-final e L)
  (if (null? L)
      (list e)
      (cons (first L) (insere-final e (rest L)))))

; any/c list? [procedure?] -> list?
(define (remove_wparams
         e l #:proc [p eqv?] #:key [k identity])
  (cond
    [(null? l) l]
    [(p (k (first l)) e)
     (remove_wparams e (rest l) #:proc p #:key k)]
    [else (cons (first l)
                (remove_wparams e (rest l) #:proc p #:key k))]
    )
  )
