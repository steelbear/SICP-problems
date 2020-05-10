#lang sicp

(define (cube x) (* x x x))

;; 1.3.1

;(define (sum-integers a b)
;  (if (> a b)
;      0
;      (+ a (sum-integers (+ a 1) b))))

;(define (sum-cubes a b)
;  (if (> a b)
;      0
;      (+ (cube a) (sum-cubes (+ a 1) b))))

;(define (pi-sum a b)
;  (if (> a b)
;      0
;      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;; exercise 1.29
; 본래 n은 매개변수가 아니지만 n의 값이 변함에 따른 값의 변화를 바로 보기 위해 매개변수로 집어넣음
(define (simpson-integral f a b n)
  (define h (* (/ (- b a) n) 1.0))
  (define (y-sign k)
    (cond ((or (= k 0) (= k n)) 1.0)
             ((even? k) 2.0)
             (else 4.0)))
  (define (y k) (* (y-sign k)
                   (f (+ a (* h k)))))
  (* (/ h 3.0)
     (sum y 0 inc n)))

;> (simpson-integral cube 0 1 100)
;0.24999999999999992
;> (simpson-integral cube 0 1 1000)
;0.2500000000000002
;> (integral cube 0 1 0.01)
;0.24998750000000042
;> (integral cube 0 1 0.001)
;0.249999875000001
;> (integral cube 0 1 0.00001)
;0.24999999998662892
;> (integral cube 0 1 0.0000001)
;Interactions disabled; out of memory

;; exercise 1.30
;(define (sum term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a) (+ result (term a)))))
;  (iter a 0))

;; exercise 1.31
;; a)
(define (product f a next b)
  (if (> a b)
      1
      (* (f a)
         (product f (next a) next b))))

(define (divide a b)
  (/ (- a (remainder a b))
     b))

(define (pi-numer x)
    (* 2 (+ (divide x 2) 1)))
  (define (pi-denom x)
    (+ (* 2 (divide (+ x 1) 2)) 1))

(define (pi-n n)
  (define (pi-numer x)
    (* 2 (+ (divide x 2) 1)))
  (define (pi-denom x)
    (+ (* 2 (divide (+ x 1) 2)) 1))
  (* (/ (product pi-numer 1 inc n)
        (product pi-denom 1 inc n))
     4.0))

;> (pi-n 10)
;3.2751010413348074
;> (pi-n 100)
;3.1570301764551676

;; b)
;(define (product f a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a)
;              (* result (f a)))))
;  (iter a 1))

;; exercise 1.32
;; a)
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

;(define (sum term a next b)
;  (accumulate + 0 term a next b))

;(define (product term a next b)
;  (accumulate * 1 term a next b))

;; b)
;(define (accumulate combiner null-value term a next b)
;  (define (iter a result)
;    (if (> a b)
;        result
;        (iter (next a)
;              (combiner result (term a)))))
;  (iter a null-value))

;; exercise 1.33
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (predicate a)
            (iter (next a)
                  (combiner result (term a)))
            (iter (next a)
                  result))))
  (iter a null-value))

;; a)
; 문제대로 있다고 가정함
(define (prime? x) #t)

(define (sum-prime-cubes a b)
  (filtered-accumulate prime? + 0 cube a inc b))

;; b)
(define (gcd-one? x) #t)

;(define (gcd-one? i)
;  (= (gcd i n) 1))

(define (sum-gcd-one n)
  (filtered-accumulate gcd-one? * 1 identity 0 inc n))

;; 1.3.2

;; exercise 1.34
(define (f g)
  (g 2))

;(f f)
;(f 2)
;(2 2)
; application: not a procedure;
; expected a procedure that can be applied to arguments
;  given: 2
;  arguments...:

;; 1.3.3

(define (average a b) (/ (+ a b) 2.0))

(define tolerance 0.001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign" a b)))))