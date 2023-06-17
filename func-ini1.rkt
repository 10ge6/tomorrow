#lang racket

;; testa se algo é um átomo
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))


;; testa se algo não é um átomo
(define (not-atom? x) (not (atom? x)))


;; testa se l é uma lista de átomos
(define (lat? l)
  (cond
    [(null? l) #t]
    [(not-atom? (first l)) #f]
    [else (lat? (rest l))]
    ))


;; testa de a é membro de l
(define (membro? a l)
  (cond
    [(null? l) #f]
    [(eqv? a (first l)) #t]
    [else (membro? a (rest l))]
    ))
 

;; soma todos os múmeros da lista l
(define (soma-numeros-v1_odd l)
  (cond
    [(null? l) 0]
    [else (+ (first l)
             (soma-numeros-v1_odd (rest l)))]
    ))

;; soma todos os múmeros da lista l
; dobra a direita (+ e1 (+ e2 (+ e3 ... (+ en 0))))
(define (soma-numeros_v1 l)
  (if (null? l)
      0
      (+ (first l)
         (soma-numeros_v1 (rest l)))
      ))

;; multiplica todos os múmeros da lista l
; dobra a direita (* e1 (* e2 (* e3 ... (* en 1))))
(define (mul-numeros l)
  (if (null? l)
      1
      (* (first l)
         (mul-numeros (rest l)))
      ))


;; soma todos os múmeros da lista l
; dobra esquerda (+ en ... (+ e3 (+ e2 (+ e1 0))))
; usa um acumulador e é recursiva de cauda

(define (soma-numeros_v2 l)
  (sn_v2_aux l 0))

(define (sn_v2_aux l a)
  (if (null? l)
      a
      (sn_v2_aux (rest l) (+ (first l) a))))

;; inverte a lista é
; também dobra a esquerda
; (cons en ... (cons e3 (cons e2 (cons e1 '() ))))
(define (inverte l)
  (inv-aux l '()))

(define (inv-aux l a)
  (if (null? l)
      a
      (inv-aux (rest l) (cons (first l) a))))
 
; remove uma ocorrência de a em l
; dobra a direita
(define (rmembro a l)
  (cond
    [(null? l) '()]
    [(eqv? a (first l)) (rest l)]
    [else
       (cons (first l) (rmembro a (rest l)))]
    ))

 
; remove todas ocorrências de a em l
; dobra a direita
(define (rmembro-todos a l)
  (cond
    [(null? l) '()]
    [(eqv? a (first l)) (rmembro-todos a (rest l))]
    [else
       (cons (first l) (rmembro-todos a (rest l)))]
    ))

  
  
