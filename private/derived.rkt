#lang racket

(require "core.rkt")

(provide any-p
         interval
         opt
         plus
         and-p)

;; extended peg syntax

(define any-p
  (for/fold ([ac eps])
            ([i (in-range 1 256)])
    (/ (chr (integer->char i)) ac)))

(define (interval s e)
  (for/fold ([ac eps])
            ([i (in-range (char->integer s)
                          (add1 (char->integer e)))])
    (/ (chr (integer->char i)) ac)))
            
(define (opt e)
  (/ e eps))

(define (plus e)
  (<> e (star e)))

(define (and-p e)
  (not (not e)))