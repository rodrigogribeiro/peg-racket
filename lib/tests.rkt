#lang racket

(require rackunit
         rackunit/text-ui
         "peg.rkt"
         "utils.rkt")

;; some simple tests

(require rackunit/text-ui)

(define peg1
  (set-start
   (add-var empty-peg "A" (chr #\a))
   (var "A")))

(define (single e)
  (set-start empty-peg e))


(define file-tests
  (test-suite
   "Basic tests for simple PEG"
    (test-case "Test for eps"
      (check-equal?
       (run-peg (single eps) "")
       (Ok '() '())))

    (test-case "Test for chr 1"
      (check-equal?
       (run-peg (single (chr #\a)) "a")
       (Ok '(#\a) '())))

    (test-case "Test for chr 2"
      (check-equal?
       (run-peg (single (chr #\a)) "a")
       (Ok '(#\a) '())))

    (test-case "Test for chr 3"
      (check-equal?
       (run-peg (single (chr #\a)) "")
       (Fail '())))
        
    (test-case "Test for var"
      (check-equal?
       (run-peg peg1 "a")
       (Ok '(#\a) '())))

    (test-case "Test for cat 1"
       (check-equal?
         (run-peg (single ((chr #\a) . <> . (chr #\b))) "ab")
         (Ok '(#\a #\b) '())))

    (test-case "Test for cat 2"
       (check-equal?
         (run-peg (single ((chr #\a) . <> . (chr #\b))) "cb")
         (Fail '(#\c #\b))))

    (test-case "Test for cat 3"
       (check-equal?
         (run-peg (single ((chr #\a) . <> . (chr #\b))) "ac")
         (Fail '(#\a #\c))))

    (test-case "Test for choice 1"
       (check-equal?
         (run-peg (single ((chr #\a) . / . (chr #\b))) "a")
         (Ok (list #\a) '())))

    (test-case "Test for choice 2"
       (check-equal?
         (run-peg (single ((chr #\a) . / . (chr #\b))) "b")
         (Ok (list #\b) '())))

    (test-case "Test for choice 3"
       (check-equal?
         (run-peg (single ((chr #\c) . / . (chr #\d))) "b")
         (Fail (list #\b))))
    
    (test-case "Test for Star 1"
       (check-equal?
        (run-peg (single (star (chr #\a))) "aa")
        (Ok (list #\a #\a) '())))

    (test-case "Test for Star 2"
       (check-equal?
        (run-peg (single (star (chr #\a))) "aa")
        (Ok (list #\a #\a) '())))

    (test-case "Test for Star 3"
       (check-equal?
        (run-peg (single (star (chr #\a))) "aab")
        (Ok (list #\a #\a) (list #\b))))
    
    (test-case "Test for Star 4"
       (check-equal?
        (run-peg (single (star (chr #\a))) "cab")
        (Ok '() (list #\c #\a #\b))))

    (test-case "Test for Not 1"
       (check-equal?
        (run-peg (single (not (chr #\b))) "ab")
        (Ok '() (list #\a #\b))))

    (test-case "Test for Not 2"
       (check-equal?
        (run-peg (single (not (chr #\a))) "a")
        (Fail (list #\a))))
    ))

(run-tests file-tests)

(define-peg ("A" <-- (eps . / . (var "B")))
            ("B" <-- (chr #\b)))
