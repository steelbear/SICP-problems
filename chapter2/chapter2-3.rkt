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
        ((and (pair? a)
              (pair? b))
         (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        ((and (symbol? a)
              (symbol? b))
         (eq? a b))
        ((and (number? a)
              (=number? a b))
         (= a b))
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

;; 2.3.3
;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((equal? x (car set)) #t)
;        (else (element-of-set? x (cdr set)))))

;(define (adjoin-set x set)
;  (if (element-of-set? x set)
;      set
;      (cons x set)))

;(define (intersection-set set1 set2)
;  (cond ((or (null? set1) (null? set2)) '())
;        ((element-of-set? (car set1) set2)
;         (cons (car set1)
;               (intersection-set (cdr set1) set2)))
;        (else (intersection-set (cdr set1) set2))))

;; exercise 2.59
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((not (element-of-set? (car set1) set2))
;         (cons (car set1) (union-set (cdr set1) set2)))
;        (else
;         (union-set (cdr set1) set2))))

;; exercise 2.60
;(define (adjoin-set x set)
;  (cons x set))

;(define (union-set set1 set2)
;  (append set1 set2))

; ajoin-set이나 union-set을 자주 사용할 때

;; 2.3.3 (continue)
;(define (element-of-set? x set)
;  (cond ((null? set) #f)
;        ((= x (car set)) #t)
;        ((> x (car set)) #f)
;        ((< x (car set))
;         (element-of-set? x (cdr set)))))

;(define (intersection-set set1 set2)
;  (if (or (null? set1) (null? set2))
;      '()
;      (let ((x1 (car set1)) (x2 (car set2)))
;        (cond ((= x1 x2)
;               (cons x1
;                     (intersection-set (cdr set1)
;                                       (cdr set2))))
;              ((< x1 x2)
;               (intersection-set (cdr set1) set2))
;              ((> x1 x2)
;               (intersection-set set1 (cdr set2)))))))

;; exercise 2.61
;(define (adjoin-set x set)
;  (cond ((null? set) (list x))
;        ((> x (car set))
;         (cons (car set)
;               (adjoin-set x (cdr set))))
;        ((= x (car set)) set)
;        ((< x (car set)) (cons x set))))

;adjoin-set을 구현할 때 순서있는 리스트의 장점은
;굳이 set에 x가 있는지 먼저 확인할 필요가 없다는 점이다.
;순서있는 리스트를 사용한다면 element-of-set?을 통해 확인하는 대신에
;x와 (car set)의 대소비교를 통해
;x가 set에 이미 포함되어있는지, 그리고 어디에 x를 집어넣어야하는지
;재귀를 통해 확인이 가능하다.
;그래서 평균적으로 n/2정도의 단계를 거치게 된다.

;; exercise 2.62
;(define (union-set set1 set2)
;  (cond ((null? set1) set2)
;        ((null? set2) set1)
;        (else
;         (let ((x1 (car set1)) (x2 (car set2)))
;           (cond ((= x1 x2)
;                  (cons x1
;                        (union-set (cdr set1)
;                                   (cdr set2))))
;                 ((< x1 x2)
;                  (cons x1
;                        (union-set (cdr set1)
;                                   set2)))
;                 ((> x1 x2)
;                  (cons x2
;                        (union-set set1
;                                   (cdr set2)))))))))

;; 2.3.3 (continue)
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

;; exercise 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list  tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define tree->list tree->list-1)
;; a)
; 같다. left-tree, entry, right-tree 순서는 서로 같다.
;; b)
; 둘다 O(log_2 n)

;; exercise 2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;원소 리스트와 트리에 쓰일 원소 개수를 매개변수로 받으면
;먼저 left-tree를 구한다.
;left-tree는 처음에 받았던 원소 리스트와 left-tree에 쓰일 원소들의 개수(left-size)를 넘겨받으면
;만들어진 left-tree와 남은 원소 리스트(non-left-elts)를 pair를 통해 받는다.(left-result)
;여기서 non-left-elts에서 entry를 얻고(this-entry) 남은 원소들로 right-tree를 만든다.
;먼저 entry까지 뽑고 남은 원소 리스트와 right-tree에 쓰일 원소들의 개수(right-size)를 구한다음
;이를 통해 right-tree를 구한다. 이때 right-tree까지 만들고 남은 원소는 remaining-elts로 내보낸다.

;; exercise 2.65
(define (union-set set1 set2)
  (define (iter list1 list2 result)
    (cond ((null? list1) (append result list2))
          ((null? list2) (append result list1))
          (else (let ((x1 (car list1))
                      (x2 (car list2)))
                  (cond ((= x1 x2)
                         (iter (cdr list1)
                               (cdr list2)
                               (append result (list x1))))
                        ((> x1 x2)
                         (iter list1
                               (cdr list2)
                               (append result (list x2))))
                        ((< x1 x2)
                         (iter (cdr list1)
                               list2
                               (append result (list x1)))))))))
  (list->tree (iter (tree->list set1)
                    (tree->list set2)
                    '())))

(define (intersection-set set1 set2)
  (define (filter-same s1 s2 result)
    (if (or (null? s1) (null? s2))
         result
         (let ((x1 (car s1))
               (x2 (car s2)))
           (cond ((= x1 x2) (filter-same (cdr s1)
                                         (cdr s2)
                                         (append result (list x1))))
                 ((> x1 x2) (filter-same s1
                                         (cdr s2)
                                         result))
                 ((< x1 x2) (filter-same (cdr s1)
                                         s2
                                         result))))))
  (list->tree (filter-same (tree->list set1)
                           (tree->list set2)
                           '())))

;; 2.3.3 (continue)
(define (key record) (car record))

;(define (lookup given-key set-of-records)
;  (cond ((null? set-of-records) #f)
;        ((equal? given-key (key (car set-of-records)))
;         (car set-of-records))
;        (else (lookup given-key (cdr set-of-records)))))

;; exericse 2.66

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key
            (key (entry set-of-records)))
         (entry set-of-records))
        ((> given-key
            (key (entry set-of-records)))
         (lookup given-key (right-branch set-of-records)))
        ((< given-key
            (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))))
