#lang sicp

;; 1.2
(define (square x) (* x x))

;; 2.2
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flat-map f s)
  (accumulate append
              nil
              (map f s)))

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

;(define (real-part z) (apply-generic 'real-part z))
;(define (imag-part z) (apply-generic 'imag-part z))
;(define (magnitude z) (apply-generic 'magnitude z))
;(define (angle z) (apply-generic 'angle z))
;(define (make-from-real-imag x y)
;  ((get 'make-from-real-imag 'rectangular) x y))
;(define (make-from-mag-ang r a)
;  ((get 'make-from-mag-ang 'polar) r a))

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
  (define (augend s) (cadr s))
  (define (addend s) (caddr s))
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augend exp) var)))

  (define (tag x) (attach-tag '+ x))
  (put 'make-sum '+ make-sum)
  (put 'deriv '+
       (lambda (opr var) (deriv-sum (tag opr) var)))
  'done)

(install-sum-package)
(define make-sum (get 'make-sum '+))

(define (install-product-package)
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))
  (define (multiplicand s) (cadr s))
  (define (multiplier s) (caddr s))
  (define (deriv-product exp var)
    (make-sum
     (make-product (multiplier exp)
                   (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
                   (multiplicand exp))))

  (define (tag x) (attach-tag '* x))
  (put 'make-product '* make-product)
  (put 'deriv '*
       (lambda (opr var) (deriv-product (tag opr) var)))
  'done)

(install-product-package)
(define make-product (get 'make-product '*))

;; c)
(define (install-frac-package)
  (define (make-frac n d)
    (cond ((=number? d 0) (error "Divided by 0 -- MAKE-FRAC"))
          ((=number? d 1) n)
          ((=number? n 0) 0)
          (else (list '/ n d))))
  (define (numer f) (cadr f))
  (define (denom f) (caddr f))
  (define (deriv-frac exp var)
    (make-frac (make-sum (make-product (deriv (numer exp) var)
                                       (denom exp))
                         (make-product -1
                                       (make-product (numer exp)
                                                     (deriv (denom exp) var))))
               (make-product (denom exp)
                             (denom exp))))

  (define (tag x) (attach-tag '/ x))
  (put 'make-frac '/ make-frac)
  (put 'deriv '/
       (lambda (opr var) (deriv-frac (tag opr) var)))
  'done)

(install-frac-package)
(define make-frac (get 'make-frac '/))

;; d)
; put의 매개변수 순서가 바뀌지 않으면 바꿀 코드는 없다.
; 만약 있다면 각 타입마다 put에게 넘기는 값의 순서를 바꾸면 된다.

;; exercise 2.74
;; a)
(define (get-record name file)
  (apply-generic 'get-record name file))
; 모든 파일은 그 파일의 집합 구성 방식에 맞춰 get-record라는 프로시저를 가지고 있어야 한다.
; 이 프로시저는 이름과 파일을 받아 해당 이름과 같은 이름을 가진 레코드의 리스트를 결과로 준다.
; 각 파일은 태그를 가지고 있어야 하며, 해당 태그는
; 파일 구성 방식이나 부서 이름으로 동일한 주제의 이름으로 정해져야 한다.

;; b)
(define (get-salary record)
  (apply-generic 'get-salary record))
; 각 레코드는 부서 이름으로 된 태그를 가지고있어야 한다.
; 또한 어떤 레코드든 이름과 봉급 정보를 무조건 가지고 있어야 한다.

;; c)
(define (find-employee-record name database)
  (flat-map (lambda (file)
              (get-record name file))
            database))

;; d)
; 해당 시스템을 그대로 가져온 다음
; 그 시스템을 통해 get-record와 get-salary를 정의한다.
; 그리고 각 파일과 레코드에 해당 시스템의 태그를 붙인 다음
; put을 통해 해당 시스템일 때 적절한 프로시저를 찾도록 등록한다.

;; 2.4.3 (continue)
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan x y))
          (else (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

;(define (apply-generic op arg) (arg op))

;; exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;; exercise 2.76
; 일을 직접 나눠 맡기는 경우
; - 타입이나 연산을 추가하면 제너릭 프로시저를 수정해야 한다.
; - 모든 타입을 알고 있어야 한다.
; 데이터 중심 프로그래밍
; - 타입을 추가해도 해당 제너릭 프로시저는 수정할 필요가 없다.
; - 연산을 추가할 때 apply-generic이나 get을 통해 정의할 수 있으므로
;   그 연산에 해당되는 모든 연산을 기억할 필요가 없다.
; 메시지 패싱
; - 타입을 추가하기만 하면 된다.
; - 타입을 구분하기 위한 태그를 붙일 필요가 없다.
; - 연산을 추가할 때는 필요한 타입에서 정의하기만 하면 쓸 수 있다.

; 타입 추가 = 데이터 중심 프로그래밍
;            -> 다른 코드에서 정의한 부분을 그대로 사용이 가능하다.
; 연산 추가 = 메시지 패싱
;            -> 저장할 데이터를 바로 사용할 수 있다.
