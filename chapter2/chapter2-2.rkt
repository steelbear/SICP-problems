#lang sicp

;; 1
(define (square x)
  (* x x))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (prime? n)
  (= n (smallest-divisor n)))

;; 2.2.1
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

; commented for 2.33
;(define (length items)
;  (if (null? items)
;      0
;      (+ 1 (length (cdr items)))))

;(define (length items)
;  (define (length-iter a count)
;    (if (null? a)
;        count
;        (length-iter (cdr a) (+ 1 count))))
;  (length-iter items 0))

; commented for 2.33
;(define (append list1 list2)
;  (if (null? list1)
;      list2
;      (cons (car list1) (append (cdr list1) list2))))

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
; commented for 2.35
;(define (count-leaves x)
;  (cond ((null? x) 0)
;        ((not (pair? x)) 1)
;        (else (+ (count-leaves (car x))
;                 (count-leaves (cdr x))))))

;; exercise 2.28
(define (fringe tree)
  (cond ((null? tree) nil)
        ((pair? (car tree)) (append (fringe (car tree))
                                    (fringe (cdr tree))))
        (else (cons (car tree)
                    (fringe (cdr tree))))))

;; exercise 2.29
; commented for d)
;(define (make-mobile left right)
;  (list left right))
;
;(define (make-branch length structure)
;  (list length structure))

;; a)
; commented for d)
(define (left-branch mobile) (car mobile))
;(define (right-branch mobile) (cadr mobile))
(define (branch-length branch) (car branch))
;(define (branch-structure branch) (cadr branch))

;; b)
(define (total-weight mobile)
  (cond ((null? mobile) 0)
        ((pair? mobile) (+ (total-weight (branch-structure (left-branch mobile)))
                           (total-weight (branch-structure (right-branch mobile)))))
        (else mobile)))

;; c)
(define (balanced? mobile)
  (define (branch-torque branch)
    (* (branch-length branch)
       (total-weight (branch-structure branch))))
  (or (not (pair? mobile))
      (let ((left-mobile (branch-structure (left-branch mobile)))
            (right-mobile (branch-structure (right-branch mobile))))
        (and (balanced? left-mobile)
             (balanced? right-mobile)
             (= (branch-torque (left-branch mobile))
                (branch-torque (right-branch mobile)))))))

;; d)
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

; list가 cons로 바뀌었다면
; cadr을 cdr로 바뀌기만 해도 작동한다.
(define (right-branch mobile) (cdr mobile))
(define (branch-structure branch) (cdr branch))

;; exercise 2.30
;(define (square-tree tree)
;  (cond ((null? tree) nil)
;        ((not (pair? tree)) (square tree))
;        (else (cons (square-tree (car tree))
;                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (square-tree subtree)
             (square subtree)))
       tree))

;; exercise 2.31
(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map f subtree)
             (f subtree)))
       tree))

;; exercise 2.32
(define (subsets s)
  (if (null? s)
      (list nil) ; 더이상 조합할 원소가 없다면 공집합
      (let ((rest (subsets (cdr s))))
        (append rest ; 맨 앞의 원소를 제외한 원소들로 부분집합 구하기
                (map (lambda (e) (cons (car s) e)) ; rest에 있는 부분집합에
                     rest)))))                     ; 제외했던 맨 앞 원소를 집어넣기

;; 2.2.3 (continue)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;; exercise 2.33
;(define (map p sequence)
;  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq1 seq2))

(define (length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

;; exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* higher-terms x) this-coeff))
              0
              coefficient-sequence))

;; exercise 2.35
(define (count-leaves t)
  (accumulate + 0 (map (lambda (subtree)
                         (if (not (pair? subtree))
                             1
                             (count-leaves subtree)))
                       t)))

;; exercise 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; exercise 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v)
           (map (lambda (w) (dot-product v w)) cols))
         m)))

;; exericse 2.38
(define (fold-right op initial sequence)
  (accumulate op initial sequence))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; > (fold-right / 1 (list 1 2 3))
; 1 1/2
; > (fold-left / 1 (list 1 2 3))
; 1/6
; > (fold-right list nil (list 1 2 3))
; (1 (2 (3 ())))
; > (fold-left list nil (list 1 2 3))
; (((() 1) 2) 3)

; 피연산자의 위치에 영향을 받는 연산자면
; fold-left와 fold-right의 결과는 다르게 나온다.
; 하지만 피연산자의 위치가 상관없는 연산자라면
; fold-left와 fold-right의 결과는 같다.

;; exercise 2.39
;(define (reverse sequence)
;  (fold-right (lambda (x y)
;                (append y (list x)))
;              nil
;              sequence))

;(define (reverse sequence)
;  (fold-left (lambda (x y) (cons y x)) nil sequence))

;; 2.2.3 (continue)
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;; exercise 2.41
(define (triple-sum s n)
  (define (sum-of-three items)
    (+ (car items) (cadr items) (caddr items)))
  (filter (lambda (l) (= (sum-of-three l) s))
          (flatmap (lambda (i)
                     (flatmap (lambda (j)
                                (map (lambda (k) (list i j k))
                                     (enumerate-interval 1 (- j 1))))
                              (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

;; exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (make-queen k r) (cons k r))
(define (queen-k q) (car q))
(define (queen-r q) (cdr q))

(define empty-board nil)

(define (adjoin-position new-row k positions)
  (cons (make-queen k new-row)
        positions))

(define (safe? k positions)
  (let ((last-queen (car positions))
        (except-last-queen (cdr positions)))
    (define (safe-row? k positions)
      (accumulate (lambda (x y) (and x y))
                  #t
                  (map (lambda (q)
                         (not (= (queen-r q) (queen-r last-queen))))
                       except-last-queen)))
    (define (safe-diagonal? k positions)
      (accumulate (lambda (x y) (and x y))
                  #t
                  (append (map (lambda (q)
                                 (not (= (- (queen-r q) (- (queen-k q) k))
                                         (queen-r last-queen))))
                               except-last-queen)
                          (map (lambda (q)
                                 (not (= (+ (queen-r q) (- (queen-k q) k))
                                         (queen-r last-queen))))
                               except-last-queen))))
    (and (safe-row? k positions)
         (safe-diagonal? k positions))))

;; exercise 2.43
; Louis가 짠 방식으로 설계하면
; 퀸을 놓는 한가지 가능성을 계산할 때마다
; 다음 줄에 놓을 수 있는 모든 가능성을 계산해야 한다.
; 즉, 한 줄에 퀸을 놓을 수 있는 경우의 수를 구할 때
; (queens (- n 1))은 8번 계산된다.

; 하지만 위에서 제시한 방식으로 계산하면
; 미리 그 줄에 놓을 수 있는 모든 가능성을 구한 후
; 다음 줄에 놓을 수 있는 여덟가지 가능성을 놓아본다.
; 즉, 한 줄에 퀸을 놓을 수 있는 경우의 수를 구할 때
; (queens (- n 1))은 한번만 계산된다.

; 위의 방법대로 (queens 8)을 구하는데 걸리는 시간을 T라고 하면
; Louis의 방법은 8T의 시간이 걸린다.
