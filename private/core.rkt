#lang racket

(require (for-syntax syntax/parse)
         (for-syntax racket)
         (for-syntax racket/syntax))
(require rackunit )
(require "utils.rkt")

(provide run-peg
         empty-peg
         add-var
         set-start
         eps
         chr
         var
         /
         <>
         star
         not
         define-peg)


;; definition of a simple parsing expression
;; deep embedding

(struct Eps
  (emp)
  #:transparent)

(struct Chr
  (chr)
  #:transparent)

(struct Var
  (non-term)
  #:transparent)

(struct Choice
  (left right)
  #:transparent)

(struct Cat
  (left right)
  #:transparent)

(struct Star
  (expr)
  #:transparent)

(struct Not
  (expr)
  #:transparent)


;; our smart constructors

(define eps
  (Eps '()))

(define (chr c)
  (Chr c))

(define (var v)
  (Var v))

(define (/ e1 e2)
  (Choice e1 e2))

(define (<> e1 e2)
  (Cat e1 e2))

(define (star e)
  (Star e))

(define (not e)
  (Not e))


;; definition of the grammar interface

(struct peg
  (rules start)
  #:transparent)

(define empty-peg
  (peg '() '()))

(define (add-var g v e)
  (match g
    [(peg rs st)
     (peg (dict-set rs v e) st)]))

(define (lookup-var g v)
  (match g
    [(peg rs st)
     (dict-ref rs v)]))

(define (set-start g p)
  (peg (peg-rules g) p))

;; automating the grammar creation

(define-syntax define-peg
  (syntax-rules (<--)
    [(define-peg) (empty-peg)]
    [(define-peg [nt <-- expr]) (peg (list (cons nt expr)) (Var nt))]
    [(define-peg [nt <-- expr] rest) (set-start (add-var (define-peg rest) nt expr) (Var nt))]))

;; peg interpreter

(define (run-parsing-exp g p s)
  (match p
    [(Eps _) (Ok '() s)]
    [(Chr c)
     (match s
       [(cons c1 s1)
        (if (char=? c c1)
            (Ok (cons c '()) s1)
            (Fail s))]
       ['() (Fail s)])]
    [(Var v)
     (run-parsing-exp g (lookup-var g v) s)]
    [(Cat p1 p2)
     (match (run-parsing-exp g p1 s)
       [(Ok pre suf)
        (match (run-parsing-exp g p2 suf)
          [(Ok pre1 suf1) (Ok (append pre pre1) suf1)]
          [(Fail _) (Fail s)])]
       [(Fail _) (Fail s)])]
    [(Choice p1 p2)
     (match (run-parsing-exp g p1 s)
       [(Ok pre suf) (Ok pre suf)]
       [(Fail _) (run-parsing-exp g p2 s)])]
    [(Star p)
     (match (run-parsing-exp g p s)
       [(Ok pre suf)
        (match (run-parsing-exp g (Star p) suf)
          [(Ok pre1 suf1) (Ok (append pre pre1) suf1)]
          [(Fail _) (Ok pre suf)])]
       [(Fail _) (Ok '() s)])]
    [(Not p)
     (match (run-parsing-exp g p s)
       [(Ok pre suf) (Fail s)]
       [(Fail _) (Ok '() s)])]))

(define (run-peg g s)
  (run-parsing-exp g (peg-start g)
                     (string->list s)))

