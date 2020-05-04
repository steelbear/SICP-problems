#lang sicp

;; exercise 1.9
(define (plus-rec a b)
  (if (= a 0)
      b
      (inc (plus-rec (dec a) b))))
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

(define (plus-iter a b)
  (if (= a 0)
      b
      (plus-iter (dec a) (inc b))))
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9


;; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
; = 1024
(A 2 4)
; = 65536
(A 3 3)
; = 65536

(define (f n) (A 0 n))
; f(n) = 2n
(define (g n) (A 1 n))
; g(n) = 2^n
(define (h n) (A 2 n))
; h(n) = 2^h(n-1)
; h(1) = 2
