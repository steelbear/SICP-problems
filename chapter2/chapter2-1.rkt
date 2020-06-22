#lang sicp

;; 1
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (avg x y)
  (/ (+ x y) 2.0))

(define (square x)
  (* x x))

(define (fast-expt x n)
  (cond ((= n 0) 1)
        ((even? n) (fast-expt (square x) (/ n 2)))
        (else (* x (fast-expt x (- n 1))))))

;; 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

;(define (make-rat n d)
;  (let ((g (gcd n d)))
;    (cons (/ n g) (/ d g))))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;; exercise 2.1
(define (make-rat n d)
  (let ((absn (abs n))
        (absd (abs d))
        (g (gcd (abs n) (abs d))))
    (cons (if (> (* n d) 0)
              (/ absn g)
              (- (/ absn g)))
          (/ absd g))))

;; exercise 2.2
(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (midpoint-segment seg)
  (make-point (avg (x-point (start-segment seg))
                   (x-point (end-segment seg)))
              (avg (y-point (start-segment seg))
                   (y-point (end-segment seg)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; exercise 2.3
(define (make-quad a b c d)
  (cons a (cons b (cons c d))))

(define (a-quad q)
  (car q))

(define (b-quad q)
  (car (cdr q)))

(define (c-quad q)
  (car (cdr (cdr q))))

(define (d-quad q)
  (car (cdr (cdr (cdr q)))))

(define (area-quad q)
  (let ((pos (+ (* (x-point (a-quad q)) (y-point (b-quad q)))
                (* (x-point (b-quad q)) (y-point (c-quad q)))
                (* (x-point (c-quad q)) (y-point (d-quad q)))))
        (neg (+ (* (x-point (b-quad q)) (y-point (a-quad q)))
                (* (x-point (c-quad q)) (y-point (b-quad q)))
                (* (x-point (d-quad q)) (y-point (c-quad q))))))
    (/ (abs (- pos neg)) 2.0)))

(define (perimeter-quad q)
  (define (length a b)
    (sqrt (+ (square (abs (- (x-point a) (x-point b))))
             (square (abs (- (y-point a) (y-point b)))))))
  (let ((l1 (length (a-quad q) (b-quad q)))
        (l2 (length (b-quad q) (c-quad q)))
        (l3 (length (c-quad q) (c-quad q)))
        (l4 (length (d-quad q) (a-quad q))))
    (+ l1 l2 l3 l4)))

;; exercise 2.4

;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; exericse 2.5
(define (tt-cons x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))

(define (tt-car l)
  (if (= (remainder l 2) 0)
      (+ 1 (tt-car (/ l 2)))
      0))

(define (tt-cdr l)
  (if (= (remainder l 3) 0)
      (+ 1 (tt-cdr (/ l 3)))
      0))

;; exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add-church a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

;; 2.1.4
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

;; exercise 2.7
(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))
(define (lower-bound x) (car x))

;; exercise 2.8
(define (sub-interval a b)
  (let ((p1 (- (lower-bound a) (lower-bound b)))
        (p2 (- (lower-bound a) (upper-bound b)))
        (p3 (- (upper-bound a) (lower-bound b)))
        (p4 (- (upper-bound a) (upper-bound b))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
                   
;; exercise 2.10
(define (safe-div-interval a b)
  (if (or (= (lower-bound b) 0)
          (= (upper-bound a) 0))
      (error "safe-div-interval -- cannot divide by 0")
      (div-interval a b)))

;; exercise 2.11
(define (mul2-interval a b)
  (cond ((and (> (lower-bound a) 0)
              (> (lower-bound b) 0))
         (make-interval (* (lower-bound a) (lower-bound b))
                        (* (upper-bound a) (upper-bound b))))
        ((and (< (lower-bound a) 0)
              (> (upper-bound a) 0)
              (> (lower-bound b) 0))
         (make-interval (* (lower-bound a) (upper-bound b))
                       (* (upper-bound a) (upper-bound b))))
        ((and (> (lower-bound a) 0)
              (< (lower-bound b) 0)
              (> (upper-bound b) 0))
         (make-interval (* (upper-bound a) (lower-bound b))
                        (* (upper-bound a) (upper-bound b))))
        ((and (< (lower-bound a) 0)
              (> (upper-bound a) 0)
              (< (lower-bound b) 0)
              (> (upper-bound b) 0))
         (make-interval (min (* (lower-bound a) (upper-bound b))
                             (* (upper-bound a) (lower-bound b)))
                        (max (* (lower-bound a) (lower-bound b))
                             (* (upper-bound a) (upper-bound b)))))
        ((and (< (upper-bound a) 0)
              (< (lower-bound b) 0)
              (> (upper-bound b) 0))
         (make-interval (* (upper-bound a) (upper-bound b))
                        (* (upper-bound a) (lower-bound b))))
        ((and (< (lower-bound a) 0)
              (> (upper-bound a) 0)
              (< (upper-bound b) 0))
         (make-interval (* (upper-bound a) (upper-bound b))
                        (* (lower-bound a) (upper-bound b))))
        (else (make-interval (* (lower-bound a) (lower-bound b))
                             (* (upper-bound a) (upper-bound b))))))