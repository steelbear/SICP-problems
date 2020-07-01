#lang sicp

;; 1
(define (square x)
  (* x x))

;; 2.2.1
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;(define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))))

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;; exercise 2.17
(define (last-pair items)
  (if (null? (cdr items))
      items
      (last-pair (cdr items))))

;; exercise 2.18
(define (reverse items)
  (define (reverse-iter items result)
    (if (null? items)
        result
        (reverse-iter (cdr items)
                      (cons (car items)
                            result))))
  (reverse-iter items nil))

;; exercise 2.19
(define (first-denomination coin-values)
  (car coin-values))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (no-more? coin-values)
  (null? coin-values))

; 덧셈은 피연산자의 순서를 바꿔도 결과값이 동일하다.
; 또한 cc 내에서 동전을 사용하는 모든 방법을 탐색하므로
; coin-value 내의 값들의 순서가 바뀌어도 결과에 영향을 주지 않는다.

;; exercise 2.20
(define (same-parity . items)
  (define (filter p items)
    (cond ((null? items) nil)
          ((p (car items)) (cons (car items)
                                 (filter p (cdr items))))
          (else (filter p (cdr items)))))
  (define (odd? x) (not (even? x)))
  (if (even? (car items))
      (filter even? items)
      (filter odd?  items)))

;; exercise 2.21
;(define (square-list items)
;  (if (null? items)
;      nil
;      (cons (square (car items))
;            (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;; exercise 2.22
; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons (square (car things))
;                     answer))))
;   (iter items nil))
; 해당 정의대로 계산한다면
; things의 첫 요소는 answer의 car부분으로 들어가게 되고
; 이는 things의 나머지 요소를 집어넣을수록 뒤로 밀리게 된다.
; 따라서 things의 맨 첫 요소는 answer의 맨 뒷 요소를 들어가게 되고
; 결국 원했던 결과에서 뒤집어진 모습을 보게된다.

; (define (square-list items)
;   (define (iter things answer)
;     (if (null? things)
;         answer
;         (iter (cdr things)
;               (cons answer
;                     (square (car things))))))
;   (iter items nil))
; 본래 리스트는 car에 값을, cdr은 리스트를 집어넣기로 약속했다.
; 하지만 이 정의에서는 cdr에 값을 집어넣었다.
; 따라서 해당 정의대로 계산하면 예상과는 다른 값이 나올 것이다.

;; exercise 2.23
(define (for-each f items)
  (define (do-recursive f items)
    (f (car items))
    (for-each f (cdr items)))
  (if (not (null? items))
      (do-recursive f items)))

;; exercise 2.24
; (1 (2 (3 4)))
; 그림은 생략

;; exericse 2.25
(define ex1 (list 1 3 (list 5 7) 9))
; > (car (cdr (car (cdr (cdr ex1)))))
; 7

(define ex2 (list (list 7)))
; > (car (car ex2))
; 7

(define ex3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
; > (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr ex3))))))))))))
; 7

;; exercise 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))
; > (append x y)
; (1 2 3 4 5 6)
; > (cons x y)
; ((1 2 3) 4 5 6)
; > (list x y)
; ((1 2 3) (4 5 6))

;; exercise 2.27
(define (deep-reverse seq)
  (define (iter list1 list2)
    (cond ((null? list1) list2)
          ((pair? (car list1)) (iter (cdr list1)
                                     (cons (deep-reverse (car list1))
                                           list2)))
          (else (iter (cdr list1)
                      (cons (car list1)
                            list2)))))
  (iter seq nil))

;; 2.2.2
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree)) (append (fringe (car tree))
                                    (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))
