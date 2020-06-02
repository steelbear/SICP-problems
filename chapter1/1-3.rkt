#lang sicp

(define (square x) (* x x))

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

;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v2)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess))

;; exercise 1.35

;; phi(황금비)는 x^2 = x + 1의 근이고 0이 아니므로 앞의 식에서 x로 나눠주면
;; x = 1 + 1/x가 되는데 이때 만족하는 x의 값은 phi(황금비)가 된다.
;; 따라서 \x -> 1 + 1/x의 고정점은 x = phi다.

;; > (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;; 1.6181818181818182

;; exercise 1.36

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (display-and-try guess next)
    (display guess)
    (newline)
    (if (close-enough? guess next)
          next
          (try next)))
  (define (try guess)
    (let ((next (f guess)))
      (display-and-try guess next)))
  (try first-guess))

;; 그냥 돌릴 때
; > (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; 2.0
; 9.965784284662087
; 3.004472209841214
; 6.279195757507157
; 3.759850702401539
; 5.215843784925895
; 4.182207192401397
; 4.8277650983445906
; 4.387593384662677
; 4.671250085763899
; 4.481403616895052
; 4.6053657460929
; 4.5230849678718865
; 4.577114682047341
; 4.541382480151454
; 4.564903245230833
; 4.549372679303342
; 4.559606491913287
; 4.552853875788271
; 4.557305529748263
; 4.554369064436181
; 4.556305311532999
; 4.555028263573554
; 4.555870396702851
;; 총 24번

;; average damping할 때
; > (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)
; 2.0
; 5.9828921423310435
; 4.922168721308343
; 4.628224318195455
; 4.568346513136242
; 4.5577305909237005
; 4.555909809045131
; 4.555599411610624
;; 총 8번

;; exercise 1.37
;; a)
;(define (cont-frac n d k)
;  (define (rec n d i k)
;    (if (> i k)
;        0
;        (/ (n i)
;           (+ (d i)
;              (rec n d (+ 1 i) k)))))
;  (rec n d 1 k))

(define (display-cont-frac from to)
  (define (display-n-go k)
    (display (cont-frac
              (lambda (x) 1.0)
              (lambda (x) 1.0)
              k))
    (newline)
    (display-cont-frac (+ from 1) to))
  (if (or (< from to)
          (= from to))
      (display-n-go from)))

; > (display-cont-frac 1 5)
; 1.0
; 0.5
; 0.6666666666666666
; 0.6000000000000001
; 0.625
;; k = 5 이후로는 소수 4자리 이상이다.
;; 따라서 k = 5일때 소수점 4자리까지 맞아떨어진다.

;; b)
(define (cont-frac n d k)
  (define (iter i result)
    (if (< 0 i)
        (iter (- i 1)
              (/ (n i)
                 (+ (d i)
                    result)))
        result))
  (iter k 0))

;; exercise 1.38

(define (e-n x) 1)
(define (e-d x)
  (if (= (remainder x 3) 2)
      (* 2.0
         (+ x 1) 3)
      1.0))

(define (e k)
  (+ 2.0 (cont-frac e-n e-d k)))


;; exercise 1.39

(define (tan-cf x k)
  (/ (cont-frac (lambda (y) (square x))
                (lambda (y) (- (* 2 y) 1))
                k)
     x))

;; 1.3.4

(define (average-damp f)
  (lambda (x) (average x (f x))))

; (define (sqrt x)
;   (fixed-point (average-damp (lambda (y) (/ x y)))
;                1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

; (define (sqrt x)
;   (newton-method (lambda (y) (- (square y) x))
;                   1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

; (define (sqrt x)
;   (fixed-point-of-transform (lambda (y) (/ x y))
;                             average-dump
;                             1.0))

;; exercise 1.40

(define (dubic a b c)
  (lambda (x) (+ (cube x)
                 (* a
                    (square x))
                 (* b x)
                 c)))

;; exercise 1.41

(define (double f)
  (lambda (x) (f (f x))))

; > (((double (double double)) inc) 5)
; > (((double (lambda (x) (double (double x)))) inc) 5)
; > (((lambda (x) (double (double (double (double x))))) inc) 5)
; > ((double (double (double (double inc)))) 5)
; > ((double (double (double (lambda (x) (inc (inc x)))))) 5)
; > ((double (double (lambda (x) (inc (inc (inc (inc x))))))) 5)
; > ((double (lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc x)))))))))) 5)
; > ((lambda (x) (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc x))))))))))))))))) 5)
; > (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc (inc 5))))))))))))))))
;; 5를 16번 1씩 더하므로
; 21

;; exercise 1.42

(define (compose f g)
  (lambda (x) (f (g x))))

;; exercise 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (compose (repeated f (- n 1))
               f)))


;; exercise 1.44

(define (smooth f)
  (define (three-average a b c)
    (/ (+ a b c) 3))
  (lambda (x)
    (three-average (f (- x dx))
                   (f x)
                   (f (+ x dx)))))

(define (n-times-smooth n)
  (repeated smooth n))

;; exercise 1.45



;; exercise 1.46

