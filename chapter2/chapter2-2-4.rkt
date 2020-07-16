#lang sicp
(#%require sicp-pict)

;; 2.2.4

;(define (flipped-pairs painter)
;  (let ((painter2 (beside painter (flip-vert painter))))
;    (below painter2 painter2)))

;(define (right-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (right-split painter (- n 1))))
;        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split painter (- n 1))))
        (below (beside painter right) (beside up corner)))))


;(define (square-limit painter n)
;  (let ((quarter (corner-split painter n)))
;    (let ((half (beside (flip-horiz quarter) quarter)))
;      (below (flip-vert half) half))))

;; exercise 2.44
;(define (up-split painter n)
;  (if (= n 0)
;      painter
;      (let ((smaller (up-split painter (- n 1))))
;        (below painter (beside smaller smaller)))))

;; 2.2.4 (continue)
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; exercise 2.45
;(define (split split1 split2)
;  (define (recur painter n)
;    (let ((smaller (recur painter (- n 1)))) ; let이 먼저 나오면 smaller값이 무엇인지 먼저 계산한다.
;      (if (= n 0)                            ; 그러면 무한루프를 돌게 된다.
;          painter
;          (split1 painter (split2 smaller smaller)))))
;  (lambda (painter n) (recur painter n)))

(define (split split1 split2)
  (define (recur painter n)
    (if (= n 0)
        painter
        (let ((smaller (recur painter (- n 1))))
          (split1 painter (split2 smaller smaller)))))
  (lambda (painter n) (recur painter n)))

(define right-split (split beside below))
(define up-split (split below beside))
