; incluir suite de testes neste mesmo arquivo
; incluir suite de testes neste mesmo arquivo
; incluir suite de testes neste mesmo arquivo

#lang racket
(require "../listas/listas2.rkt")

(provide CONJUNTO-VAZIO
         conjunto-vazio?
         conjunto?)

; (conjunto? any/c) -> boolean?
; testa se um objeto é um conjunto
(define (conjunto? o)
  (cond
    [(not (list? o)) #f]
    [(conjunto-vazio? o) #t]
    [(membro-p-n? (first o) (rest o)) #f]
    [else (conjunto? (rest o))]
    )
  )

(define CONJUNTO-VAZIO LISTA-VAZIA)

; (conjunto-vazio? conjunto?) -> boolean?
(define (conjunto-vazio? c)
  (if (eq? c CONJUNTO-VAZIO)
      #t
      #f
      )
  )

; (intersecta conjunto? conjunto?) -> boolean?
; testa se dois conjuntos possuem ao menos um elemento em comum
(define (intersecta c1 c2)
  (cond
    [(or (conjunto-vazio? c1) (conjunto-vazio? c2)) #f]
    [(membro-p-n? (first c1) c2) #t]
    [else (intersecta (rest c1) c2)]
    )
  )

; faz a interseção de dois conjuntos
(define (intersecao c1 c2)
  (cond
    [(or (conjunto-vazio? c1) (conjunto-vazio? c2)) CONJUNTO-VAZIO]
    [(membro-p-n? (first c1) c2)
     (cons (first c1)
           (intersecao (rest c1) c2))]
    [else (intersecao (rest c1) c2)]
    )
  )

; contract definition example
#|
(define/contract (intersecao c1 c2)
  (conjunto? conjunto? . -> . conjunto?)
  (cond
    [(or (conjunto-vazio? c1) (conjunto-vazio? c2)) CONJUNTO-VAZIO]
    [(membro-p-n? (first c1) c2)
     (cons (first c1)
           (intersecao (rest c1) c2))]
    [else (intersecao (rest c1) c2)]
    )
  )
|#

(define (uniao c1 c2)
  (cond
    [(conjunto-vazio? c1) c2]
    [(conjunto-vazio? c2) c1]
    [(not (membro-p-n? (first c1) c2))
     (cons (first c1)
           (uniao (rest c1) c2))]
    [else (uniao (rest c1) c2)]
    )
  )

(define (dif-conj c1 c2)
  (if (conjunto-vazio? c2) c1
      (rmembro-todos-equal (first c2) (dif-conj c1 (rest c2)))
      )
  )

(define (dif-simetrica-conj c1 c2)
  (cond
    [(conjunto-vazio? c1) c2]
    [(conjunto-vazio? c2) c1]
    [(membro-p-n? (first c1) c2)
     (dif-conj (rest c1) (rmembro-todos-equal (first c1) c2))]
    [else (cons (first c1) (dif-conj (rest c1) c2))]
    )
  )

(define (intersecao-lc GL)
  (cond
    [(conjunto-vazio? GL) CONJUNTO-VAZIO]
    [(membro-p-n? (first GL) (rest GL))
     (cons (first GL) (intersecao-lc (rest GL)))]
    [else (intersecao-lc (rest GL))]
    )
  )
