#lang racket
;; blotto.rkt
;; by R. Altenburg 2/2013
;; 
;; Assorted tools to score a Colonel Blotto game
;; based on Jonathan Partington's Colonel Blotto page
;; at: http://www1.maths.leeds.ac.uk/~pmt6jrp/personal/blotto.html
;;
;; 

(define gen (make-pseudo-random-generator))

(define (sum-list list)
  "sum all the numbers in a list"
  (foldl (λ (x y) (+ x y)) 0 list))

(define (add-one-to-car in-list)
  (append (list (+ 1 (car in-list))) (cdr in-list)))

(define (zip lst-a lst-b)
  "convert two lists to a list of lists by pairs"
  (map list lst-a lst-b))
  
(define (compare-assoc lst [score-a 0] [score-b 0])
  "takes a list of lists by pairs and compares each pair"
  (if (null? lst)
    (cond
      [(positive? (- score-a score-b)) 1]
      [(negative? (- score-a score-b)) 2]
      [else 0])
    (begin
      (let ([test (car lst)])
        (cond
          [(> (car test) (second test))
           (compare-assoc (cdr lst) (+ 1 score-a) score-b)]
          [(< (car test) (second test))
          (compare-assoc (cdr lst) score-a (+ 1 score-b))]
          [else (compare-assoc (cdr lst) score-a score-b)]
              )))))

(define (compare list-a list-b)
  "sugar for compare-assoc"
  (compare-assoc (zip list-a list-b)))


(define (random-list [total 100] [in-list '(0 0 0 0 0 0 0 0 0 0)])
  "Create a random list of ten digits that sums to 100"
  (let* ([pos (random 10 gen)]
         [heads (take in-list pos)]
         [tails (drop in-list pos)]
         )
    (if (zero? total)
        in-list
        (random-list (- total 1) (append heads (add-one-to-car tails))))
    ))


(define (single-test in-pool)
  "score the first trial in a list against the rest of the list"
  (let* ([case (car in-pool)]
        [tests (cdr in-pool)]
        [win 0]
        [loss 0]
        [tie 0]
        [result 0]
        [score (foldl (λ (x y)
             (set! result (compare case x))
             (cond
               [(zero? result) (begin
                                 (set! tie (+ 1 tie))
                                 (+ y 1))]
               [(= 1 result)   (begin
                                 (set! win (+ 1 win))
                                 (+ 2 y))]
               [else  (begin
                        (set! loss (+ 1 loss))
                        y)]))
              0 tests)])
    
    (list score case win loss tie)
    ))
  

(define (bulk-test-loop lst n [result '()])
  "called by bulk-test, not called directly"
  (if (zero? n)
      result
      (bulk-test-loop (append (cdr lst) (list (car lst))) (- n 1)
                      (append result (list (single-test lst))))
      ))

(define (bulk-test lst)
  "run the trials on a list of lists--tests each against all others"
  (bulk-test-loop lst (length lst)))

(define (clean-output out-lst)
  "convert bulk-test output to a list of test cases"
  (map (λ (x) (second x)) out-lst))

(define (fancy-report lst)
  "prints rank, score, set, win, loss, tie"
  (let ([counter 1])
    (map (λ (x) 
           (printf "~s) ~s ~s ~s ~s ~s~n" counter
                   (car x) (second x)
                   (third x) (fourth x)
                   (fifth x))
           (set! counter (+ 1 counter)))
           lst)
  'nil 
  ))

(define (hard-test lst)
  "test a list against a pre-selected allstar pool of lists"
  (sort 
   (bulk-test (append (include "allstar.txt") (list lst)))
   >
   #:key car))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  Start Here


;; test a particular arrangment against a pre-selected list
(fancy-report (hard-test '(5 12 20 3 8 6 13 21 4 8)))


;;
;; Test many random strings against the "allstar" list                
;;(map (λ (x) (displayln x))(clean-output (include "allstar.txt")))
;; (take (sort 
;;       (bulk-test (append (build-list 9900 (λ (x)(random-list))) allstar))
;;       >
;;       #:key car) 100)
  