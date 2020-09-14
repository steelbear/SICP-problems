#lang sicp

;; 1.2
(define (square x) (* x x))

;; 2.3
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

;; 2.4.1
(define (add-complex x1 x2)
  (make-from-real-imag (+ (real-part x1) (real-part x2))
                       (+ (imag-part x1) (imag-part x2))))

(define (sub-complex x1 x2)
  (make-from-real-imag (- (real-part x1) (real-part x2))
                       (- (imag-part x1) (imag-part x2))))

(define (mul-complex x1 x2)
  (make-from-mag-ang (* (magnitude x1) (magnitude x2))
                     (+ (angle x1) (angle x2))))

(define (div-complex x1 x2)
  (make-from-mag-ang (/ (magnitude x1) (magnitude x2))
                     (- (angle x1) (angle x2))))

;(define (real-part z) (car z))

;(define (imag-part z) (cdr z))

;(define (magnitude z)
;  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

;(define (angle z)
;  (atan (imag-part z) (real-part z)))

;(define (make-from-real-imag x y) (cons x y))

;(define (make-from-mag-ang r a)
;  (cons (* r (cos a)) (* r (sin a))))

;; 2.4.2

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

;(define (rectangular? z)
;  (eq? (type-tag z) 'rectangular))

;(define (polar? z)
;  (eq? (type-tag z) 'polar))


; procedures for rectangular
;(define (real-part-rectangular z) (car z))

;(define (imag-part-rectangular z) (cdr z))

;(define (magnitude-rectangular z)
;  (sqrt (+ (square (real-part-rectangular z))
;           (square (imag-part-rectangular z)))))

;(define (angle-rectangular z)
;  (atan (imag-part-rectangular z)
;        (real-part-rectangular z)))

;(define (make-from-real-imag-rectangular x y)
;  (attach-tag 'rectangular (cons x y)))

;(define (make-from-mag-ang-rectangular r a)
;  (attach-tag 'rectangular
;              (cons (* r (cos a)) (* r (sin a)))))

; procedures for polar
;(define (real-part-polar z)
;  (* (magnitude-polar z) (cos (angle-polar z))))

;(define (imag-part-polar z)
;  (* (magnitude-polar z) (sin (angle-polar z))))

;(define (magnitude-polar z) (car z))

;(define (angle-polar z) (cdr z))

;(define (make-from-real-imag-polar x y)
;  (attach-tag 'polar
;              (cons (sqrt (+ (square x) (square y)))
;                    (atan y x))))

;(define (make-from-mag-ang-polar r a)
;  (attach-tag 'polar (cons r a)))

; make general procedures
;(define (real-part z)
;  (cond ((rectangular? z)
;         (real-part-rectangular (contents z)))
;        ((polar? z)
;         (real-part-polar (contents z)))
;        (else (error "Unknown type -- REAL-PART" z))))

;(define (imag-part z)
;  (cond ((rectangular? z)
;         (imag-part-rectangular (contents z)))
;        ((polar? z)
;         (imag-part-polar (contents z)))
;        (else (error "Unknown type -- IMAG-PART" z))))

;(define (magnitude z)
;  (cond ((rectangular? z)
;         (magnitude-rectangular (contents z)))
;        ((polar? z)
;         (magnitude-polar (contents z)))
;        (else (error "Unknown type -- MAGNITUDE" z))))

;(define (angle z)
;  (cond ((rectangular? z)
;         (angle-rectangular (contents z)))
;        ((polar? z)
;         (angle-polar (contents z)))
;        (else (error "Unknown type -- ANGLE" z))))

;(define (make-from-real-imag x y)
;  (make-from-real-imag-rectangular x y))

;(define (make-from-mag-ang r a)
;  (make-from-mag-ang-polar r a))

;; 2.4.3
(define (install-rectangular-package)
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-real-imag x y) (cons x y))
  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  (define (make-from-mag-ang r a)
    (cons r a))

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;(install-rectangular-package)
;(install-polar-package)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;; exercise 2.73

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (=number? exp num)
    (and (number? exp) (= exp num)))

;; a)
; 숫자나 미지수는 따로 처리하고, 그 외의 연산은 각 타입에서 정의한 대로 계산한다.
; 숫자 데이터나 미지수는 다른 데이터와는 달리 operator와 operands를 정의하기 힘들다.

;; b)
(define (install-sum-package)
  (define (make-sum a1 a2)
     (cond ((=number? a1 0) a2)
           ((=number? a2 0) a1)
           ((and (number? a1) (number? a2)) (+ a1 a2))
           (else (list '+ a1 a2))))
  (define (augend s) (car s))
  (define (addend s) (cadr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (put 'make-sum '+ make-sum)
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplicand s) (car s))
  (define (multiplier s) (cadr s))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))


  (put 'make-product '* make-product)
  (put 'deriv '* deriv-product)
  'done)

(install-sum-package)
(install-product-package)

(define (make-sum . ops) (apply-generic 'make-sum ops))
(define (make-product . ops) (apply-generic 'make-product ops))

;; c)
(define (install-frac-package)
  (define (make-frac n d)
    (cond ((=number? d 0) (error "Divided by 0 -- MAKE-FRAC"))
          ((=number? d 1) n)
          ((=number? n 0) 0)
          (else (list '/ n d))))
  (define (numer f) (car f))
  (define (denom f) (cadr f))
  (define (deriv-frac exp var)
    (make-frac (make-sum (make-product (deriv (numer exp) var)
                                       (denom exp))
                         (make-product -1
                                       (make-product (numer exp)
                                                     (deriv (denom exp) var))))
               (make-product (denom exp)
                             (denom exp))))

  (put 'make-frac '/ make-frac)
  (put 'deriv '/ deriv-frac)
  'done)

(install-frac-package)

(define (make-frac . ops) (apply-generic 'make-frac ops))
