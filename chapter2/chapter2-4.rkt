#lang sicp

;; for put and get (use in racket)
(define *op-table* (make-hash))

(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))

(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;; 1.2
(define (square x) (* x x))

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

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


; procedures for rectangular
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; procedures for polar
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; make general procedures
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;; 2.4.3
;(define (install-rectangular-package)
;  (define (real-part z) (car z))
;  (define (imag-part z) (cdr z))
;  (define (magnitude z)
;    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
;  (define (angle z)
;    (atan (imag-part z) (real-part z)))
;  (define (make-from-real-imag x y) (cons x y))
;  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))
;
;  (define (tag x) (attach-tag 'rectangular x))
;  (put 'real-part '(rectangular) real-part)
;  (put 'imag-part '(rectangular) imag-part)
;  (put 'magnitude '(rectangular) magnitude)
;  (put 'angle     '(rectangular) angle)
;  (put 'make-from-real-imag 'rectangular
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'rectangular
;       (lambda (r a) (tag (make-from-mag-ang r a))))
;  'done)

;(define (install-polar-package)
;  (define (real-part z)
;    (* (magnitude z) (cos (angle z))))
;  (define (imag-part z)
;    (* (magnitude z) (sin (angle z))))
;  (define (magnitude z) (car z))
;  (define (angle z) (cdr z))
;  (define (make-from-real x y)
;    (cons (sqrt (+ (square x) (square y)))
;          (atan y x)))
;  (define (make-from-mag-ang-polar r a)
;    (cons r a))
;
;  (define (tag x) (attach-tag 'polar x))
;  (put 'real-part '(polar) real-part)
;  (put 'imag-part '(polar) imag-part)
;  (put 'magnitude '(polar) magnitude)
;  (put 'angle     '(polar) angle)
;  (put 'make-from-real-imag 'polar
;       (lambda (x y) (tag (make-from-real-imag x y))))
;  (put 'make-from-mag-ang 'polar
;       (lambda (r a) (tag (make-from-mag-ang r a))))
;  'done)

;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error
;           "No method for these tytpes -- APPLY-GENERIC"
;           (list op type-tags))))))

;(define (real-part z) (apply-generic 'real-part z))
;(define (imag-part z) (apply-generic 'imag-part z))
;(define (magnitude z) (apply-generic 'magnitude z))
;(define (angle z) (applay-generic 'angle z))
;(define (make-from-real-imag x y)
;  ((get 'make-from-real-imag 'rectangluar) x y))
;(define (make-from-mag-ang r a)
;  ((get 'make-from-mag-ang 'polar) r a))
