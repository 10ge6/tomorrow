#lang racket
(require rackunit)

(provide LISTA-VAZIA
         atom?
         gera-lista
         rmembro
         membro-p-n?
         rmembro-todos*
         rmembro-todos-equal
         )


(define LISTA-VAZIA '())

(define (atom? a)
  (not (or (pair? a) (null? a)))
  )

;; gera-lista
;; Recebe N elementos e gera uma lista com eles
;; any/c ... -> list?
(define (gera-lista . elementos)
  elementos)

;; rmembro e l
;; Remove a primeira ocorrência do elemento e na lista l
;; atom? list? -> list?
(define (rmembro e l)
  (cond
    [(null? l) null]
    [(eqv? e (first l)) (rest l)]
    [else  (cons (first l)
                 (rmembro e (rest l)))])
  )

;; (membro-p-n? any/c list?) -> boolean
;; Testa se o elemento e é membro da lista l
;; L é uma lista simples e E é qualuer coisa
(define (membro-p-n? e l)
  (cond
    [(null? l) #f]
    [(equal? e (first l)) #t]
    [else
       (membro-p-n? e (rest l))]
    ))

  

;; rmembro-todos e l
;; Remove todas as ocorrências do elemento e da lista l
;; atom? list? -> list?
(define (rmembro-todos e l)
  (cond
    [(null? l) null]
    [(eqv? e (first l)) (rmembro-todos e (rest l))]
    [else  (cons (first l)
                 (rmembro-todos e (rest l)))])
  )

(define (rmembro-todos-equal e l)
  (cond
    [(null? l) null]
    [(equal? e (first l)) (rmembro-todos e (rest l))]
    [else  (cons (first l)
                 (rmembro-todos e (rest l)))])
  )


;; substitui-todos a n l
;; Substitui todas as ocorrências do elemento a pelo
;; elemento n na lista l
;; atom? atom? list? -> list?
(define (substitui-todos a n l)
  (cond
    [(null? l) null]
    [(eqv? a (first l)) (cons n (substitui-todos a n (rest l)))]
    [else  (cons (first l)
                 (substitui-todos a n (rest l)))])
  )

;; membro? e l
;; Teste se o elemento e está na lista l
;; atom? list? -> boolean?
(define (membro? a l)
  (cond
    [(null? l) #f]
    [(eqv? a (first l)) #t]
    [else
     (membro? a (rest l))])
  )


;; membro*? e lg
;; Teste se o elemento e está na lista genérica lg
;; atom? list? -> boolean?
(define (membro*? a lg)
  (cond
    [(null? lg) #f]
    [(list? (first lg))
     (or (membro*? a (first lg))
         (membro*? a (rest lg)))]
    [(eqv? a (first lg)) #t]
    [else
     (membro*? a (rest lg))])
  )


;; rmembro-todos* e lg
;; Remove todas as ocorrências do elemento e da
;; lista genérica lg
;; atom? list? -> list?
(define (rmembro-todos* e lg)
  (cond
    [(null? lg) null]
    [(list? (first lg))
     (cons (rmembro-todos* e (first lg))
           (rmembro-todos* e (rest lg)))]
    [(eqv? e (first lg)) (rmembro-todos* e (rest lg))]
    [else  (cons (first lg)
                 (rmembro-todos* e (rest lg)))])
  
  )

#|
;; Testes de rmembro-todos*
(check-equal?
   (rmembro-todos* 'b '())
   '())

(check-equal?
   (rmembro-todos* 'b '(a b c))
   '(a c))

(check-equal?
   (rmembro-todos* 'b '(a b c b))
   '(a c))

(check-equal?
   (rmembro-todos* 'b '(a b c (b v)))
   '(a c (v)))

(check-equal? 
  (rmembro-todos*
    'a
    '(((ra a) b d a) (fd s (www b)) (d (a))))
  '(((ra) b d) (fd s (www b)) (d ()))
)

|#

#|
(define ts
  (test-suite
   "An example suite"
   #:before (lambda () (display "Before\n"))
   #:after  (lambda () (display "After\n"))
   (test-case
    "An example test"
    (check-eq? 1 11))
   (test-suite "A nested test suite"
               (test-case
                "Another test"
                (check < 12 2))))
  )
|#

;; insere-direita* a na lg
;; Insere na à direita de toda ocorrência de a em lg
;; atom? atom? list? -> list?
(define (insere-direita* a an lg)
  (cond
    [(null? lg) null]
    [(list? (first lg))
     (cons (insere-direita* a an (first lg))
           (insere-direita* a an (rest lg)))]
    [(eqv? a (first lg))
     (cons a (cons an (insere-direita* a an (rest lg))))]
    [else  (cons (first lg)
                 (insere-direita* a an (rest lg)))])
  )

;; inverte2 l
;; Inverte a lista l
;; list? -> list?
(define (inverte2 l)
  (inv-aux l '()))

(define (inv-aux l acc)
  (if (null? l)
      acc
      (inv-aux (rest l) (cons (first l) acc))))


;; inverte l
;; Inverte a lista l
;; list? -> list?
(define (inverte l [acc '()])
  (if (null? l)
      acc
      (inverte (rest l) (cons (first l) acc))))


;; inverte* lg
;; Inverte toda a estrutura da lg
;; list? -> list?
(define (inverte* lg)
  (inverte-aux* lg '()))

(define (inverte-aux* lg acc)
  (cond
    [(null? lg) acc]
    [(list? (first lg))
     (inverte-aux* (rest lg)
                   (cons (inverte-aux* (first lg) '()) acc))]
    [else (inverte-aux* (rest lg) (cons (first lg) acc))]
    ))


;; rmembro-uma-vez* e lg
;; Remove uma ocorrência do elemento e da
;; lista genérica lg
;; atom? list? -> list?
(define (rmembro-uma-vez* e lg)
  (let-values ([(b lr) (rmembro-uma-vez-i* e lg)])
    lr))


;; rmembro-uma-vez-i* e lg
;; Remove uma ocorrência do elemento e da
;; lista genérica lg e retorna um booleano e
;; lista resultante
;; atom? list? -> boolean? list?
(define (rmembro-uma-vez-i* e lg)
  (cond
    [(null? lg) (values #f null)]
    [(list? (first lg))
     (let-values ([(b1 lr1) (rmembro-uma-vez-i* e (first lg))])
       (if b1
           (values #t (cons lr1 (rest lg)))
           (let-values ([(b2 lr2) (rmembro-uma-vez-i* e (rest lg))])
             (values b2 (cons lr1 lr2))))
       )]
    [(eqv? e (first lg))
     (values #t (rest lg))]
    [else (let-values ([(b2 lr2) (rmembro-uma-vez-i* e (rest lg))])
            (values b2 (cons (first lg) lr2)))])
  )

;; intercala-m n elementos
;; cria uma lista de tamanho m intercalando os
;; elementos
;; integer? any/c ... -> list?
(define (intercala-m n . elementos)
  (if (< n 1)
      null
      (cons (first elementos)
            (apply intercala-m
                   (cons (- n 1)
                         (insere-final (first elementos)
                                       (rest elementos)))))))

;; insere-final e lista
;; insere e no final da lista
;; any/c List? -> list?
(define (insere-final e l)
  (if (null? l)
      (list e)
      (cons (first l) (insere-final e (rest l)))))


;; remove-p e lista [proc] [key]
;; remove e da lista
;; any/c List? [procedure?] [procedure?] -> list?
(define (remove-p 
         e l #:proc [p eqv?] #:key [k identity])
  (cond
    [(null? l) null]
    [(p (k (first l)) e)
     (remove-p e (rest l) #:proc p #:key k)]
    [else (cons (first l) 
                (remove-p e (rest l) #:proc p #:key k))]
    ))




