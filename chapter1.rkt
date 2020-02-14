#lang sicp

(define (square x) (* x x)

;;exercise 1.2
(/ (+ 5
      4
      (- 2 (- 3 (+ 6
                   (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))

;;exercise 1.3
(define (bigger-square-sum a b c)
  (if (> a b)
      (+ (square a) (square (if (> b c) b c)))
      (+ (square b) (square (if (> a c) a c)))))

;;exercise 1.7
(define (good-enough? guess x)
  (< (abs (- guess (improve guess x))) 0.001))

;;exercise 1.8
(define (improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x) x)))

(define (cbrt x)
  (cbrt-iter 1.0 x))