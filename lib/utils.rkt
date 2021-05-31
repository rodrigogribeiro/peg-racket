#lang racket

(provide (all-defined-out))

(struct Ok
  (pref suf)
  #:transparent)

(struct Fail
  (inp)
  #:transparent)