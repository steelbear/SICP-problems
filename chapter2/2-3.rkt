#lang sicp

;; 1.2

(define (square x) (* x x))

(define (fast-expt-iter b n res)
  (cond ((= n 0) res)
        ((even? n) (fast-expt-iter (square b) (/ n 2) res))
        (else (fast-expt b (- n 1) (* res b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;; 2.3.1
(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; exercise 2.53
; (list 'a 'b 'c) => (a b c)
; (list (list 'george)) => ((george))
; (cdr '((x1 x2) (y1 y2))) => ((y1 y2))
; (cadr '((x1 x2) (y1 y2))) => (y1 y2)
; (pair? (car '(a short list))) => #false
; (memq 'red '((red shoes) (blue socks))) => #false
; (memq 'red '(red shoes blue socks)) => (red shoes blue socks)

;; exercise 2.54
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((eq? (car a) (car b)) (equal? (cdr a) (cdr b)))
        (else #f)))

;; exercise 2.55
; ''abracadabra => (list 'quote 'abracadabra)

;; 2.3.2
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp)
;         (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product (multiplier exp)
;                        (deriv (multiplicand exp) var))
;          (make-product (deriv (multiplier exp))
;                        (multiplicand exp))))
;        (else
;         (error "unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;;(define (make-sum a1 a2) (list '+ a1 a2))
;(define (make-sum a1 a2)
;  (cond ((=number? a1 0) a2)
;        ((=number? a2 0) a1)
;        ((and (number? a1) (number? a2)) (+ a1 a2))
;        (else (list '+ a1 a2))))

;;(define (make-product m1 m2) (list '* m1 m2))
;(define (make-product m1 m2)
;  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
;        ((=number? m1 1) m2)
;        ((=number? m2 1) m1)
;        ((and (number? m1) (number? m2)) (* m1 m2))
;        (else (list '* m1 m2))))

;(define (sum? x)
;  (and (pair? x) (eq? (car x) '+)))

;(define (augend s) (cadr s))

;(define (addend s) (caddr s))

;(define (product? x)
;  (and (pair? x) (eq? (car x) '*)))

;(define (multiplicand s) (cadr s))

;(define (multiplier s) (caddr s))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; exercise 2.56
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp)
                                                         (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

(define (exponentiation? e)
  (and (pair? e) (eq? (car e) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

;(define (make-exponentiation base exponent)
;  (list '** base exponent))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (fast-expt b e))
        (else (list '** b e))))

;; exercise 2.57
;(define (augend e)
;  (cadr e))

;(define (addend e)
;  (if (null? (cdddr e))
;      (caddr e)
;      (cons '+ (cddr e))))

;(define (multiplicand e)
;  (cadr e))

;(define (multiplier e)
;  (if (null? (cdddr e))
;      (caddr e)
;      (cons '* (cddr e))))

;; exercise 2.58
;; a)
;술어 -> cadr로 기호 확인
;짜맞추개 -> list로 구현하되 기호 위치만 바꾼다
;고르개 -> 기호를 기준으로 왼쪽과 오른쪽으로 나눔

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;(define (sum? x)
;  (and (pair? x) (eq? (cadr x) '+)))

(define (augend s)
  (define (rec s)
    (if (eq? (car s) '+)
        nil
        (cons (car s) (rec (cdr s)))))
  (if (eq? (cadr s) '+)
      (car s)
      (rec s)))

(define (addend s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

;(define (product? x)
;  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplicand s)
  (define (rec s)
    (if (eq? (car s) '*)
        nil
        (cons (car s) (rec (cdr s)))))
  (if (eq? (cadr s) '*)
      (car s)
      (rec s)))

(define (multiplier s)
  (if (null? (cdddr s))
      (caddr s)
      (cddr s)))

;; b)
;sum?에서 주어진 식에 적어도 하나 이상의 + 기호가 있어야 하고,
;product?에서는 주어진 식에 모든 기호가 * 여야 한다

(define (sum? x)
  (cond ((null? (cdr x)) #f)
        ((eq? (cadr x) '+) #t)
        (else (sum? (cddr x)))))

(define (product? x)
  (cond ((null? (cdr x)) #t)
        ((eq? (cadr x) '*) (product? (cddr x)))
        (else #f)))