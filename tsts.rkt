#lang racket
(require rackunit)
(require rackunit/text-ui)
(require "./arithmetics.rkt")


(define L0 '())
(define L1 '(a b c))
(define L2 '(a b c b (d e b) g (b (b) (bb d b))))
(define ts
  (test-suite "testes de remove_all*"
              
           
              (check-equal? (remove_all* 'b L0) '())
              (check-equal? (remove_all* 'b L1) '(a c) "!panic")
              (check-equal? (remove_all* 'b L2) '(a c (d e) g (() (bb d))))
              )
  )

(run-tests ts)
