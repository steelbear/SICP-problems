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

;(define (corner-split painter n)
;  (if (= n 0)
;      painter
;      (let ((up (up-split painter (- n 1)))
;            (right (right-split painter (- n 1)))
;            (corner (corner-split painter (- n 1))))
;        (below (beside painter right) (beside up corner)))))


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

;(define (square-limit painter n)
;  (let ((combine4 (square-of-four flip-horiz identity
;                                  rotate180 flip-vert)))
;    (combine4 (corner-split painter n))))

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

;; 2.2.4 (continue)
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; exercise 2.46
;(define (make-vect xcor ycor) (cons xcor ycor))
;(define (xcor-vect v) (car v))
;(define (ycor-vect v) (cdr v))
;(define (add-vect v w) (make-vect (+ (xcor-vect v)
;                                     (xcor-vect w))
;                                  (+ (ycor-vect v)
;                                     (ycor-vect w))))
;(define (sub-vect v w) (make-vect (- (xcor-vect v)
;                                     (xcor-vect w))
;                                  (- (ycor-vect v)
;                                     (ycor-vect w))))
;(define (scale-vect s v) (make-vect (* s (xcor-vect v))
;                                    (* s (ycor-vect v))))
(define xcor-vect vector-xcor)
(define ycor-vect vector-ycor)
(define add-vect vector-add)
(define sub-vect vector-sub)
(define scale-vect vector-scale)

;; exercise 2.47

;(define (make-frame origin edge1 edge2)
;  (list origin edge1 edge2))

;(define (origin-frame frame) (car frame))
;(define (edge1-frame frame) (cadr frame))
;(define (edge2-frame frame) (caddr frame))

;(define (make-frame origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))

;(define (origin-frame frame) (car frame))
;(define (edge1-frame frame) (cadr frame))
;(define (edge2-frame frame) (cddr frame))

(define origin-frame frame-origin)
(define edge1-frame frame-edge1)
(define edge2-frame frame-edge2)

;; 2.2.4 (continue)
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

;; exercise 2.48
;(define (make-segment start end)
;  (cons start end))

;(define (start-segment seg) (car seg))
;(define (end-segment seg) (cdr seg))

;; exercise 2.49
;a)
(define (border-painter frame)
  (let ((p1 (make-vect 0.0 0.0))
        (p2 (make-vect 0.0 1.0))
        (p3 (make-vect 1.0 1.0))
        (p4 (make-vect 1.0 0.0)))
    ((segments->painter (list (make-segment p1 p2)
                              (make-segment p2 p3)
                              (make-segment p3 p4)
                              (make-segment p4 p1)))
     frame)))

;b)
(define (cross-painter frame)
  ((segments->painter (list (make-segment (make-vect 0.0 0.0)
                                          (make-vect 1.0 1.0))
                            (make-segment (make-vect 1.0 0.0)
                                          (make-vect 0.0 1.0))))
   frame))

;c)
(define (diamond-painter frame)
  (let ((top (make-vect 0.5 1.0))
        (bottom (make-vect 0.5 0.0))
        (left (make-vect 0.0 0.5))
        (right (make-vect 1.0 0.5)))
    ((segments->painter (list (make-segment top left)
                              (make-segment top right)
                              (make-segment bottom left)
                              (make-segment bottom right)))
     frame)))

;d)
;(define (wave frame)
;  (let ((line1 (list (make-segment (make-vect 0.0 0.8)
;                                   (make-vect 0.15 0.6))
;                     (make-segment (make-vect 0.15 0.6)
;                                   (make-vect 0.3 0.65))
;                     (make-segment (make-vect 0.3 0.65)
;                                   (make-vect 0.45 0.65))
;                     (make-segment (make-vect 0.45 0.65)
;                                   (make-vect 0.3 0.8))
;                     (make-segment (make-vect 0.3 0.8)
;                                   (make-vect 0.45 1.0))))
;        (line2 (list (make-segment (make-vect 0.55 1.0)
;                                   (make-vect 0.6 0.8))
;                     (make-segment (make-vect 0.6 0.8)
;                                   (make-vect 0.55 0.65))
;                     (make-segment (make-vect 0.55 0.65)
;                                   (make-vect 0.6 0.65))
;                     (make-segment (make-vect 0.6 0.65)
;                                   (make-vect 1.0 0.35))))
;        (line3 (list (make-segment (make-vect 1.0 0.2)
;                                   (make-vect 0.55 0.5))
;                     (make-segment (make-vect 0.55 0.5)
;                                   (make-vect 0.6 0.0))))
;        (line4 (list (make-segment (make-vect 0.45 0.0)
;                                   (make-vect 0.5 0.3))
;                     (make-segment (make-vect 0.5 0.3)
;                                   (make-vect 0.55 0.0))))
;        (line5 (list (make-segment (make-vect 0.0 0.65)
;                                   (make-vect 0.15 0.5))
;                     (make-segment (make-vect 0.15 0.5)
;                                   (make-vect 0.3 0.6))
;                     (make-segment (make-vect 0.3 0.6)
;                                   (make-vect 0.45 0.55))
;                     (make-segment (make-vect 0.45 0.55)
;                                   (make-vect 0.15 0.0)))))
;    ((segments->painter (append line1 line2 line3 line4 line5))
;     frame)))

;; 2.2.4 (continue)
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;(define (beside painter1 painter2)
;  (let ((split-point (make-vect 0.5 0.0)))
;    (let ((paint-left
;           (transform-painter painter1
;                              (make-vect 0.0 0.0)
;                              split-point
;                              (make-vect 0.0 1.0)))
;          (paint-right
;           (transform-painter painter2
;                              split-point
;                              (make-vect 1.0 0.0)
;                              (make-vect 0.5 1.0))))
;      (lambda (frame)
;        (paint-left frame)
;        (paint-right frame)))))

;; exercise 2.50
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; exercise 2.51
;(define (below painter1 painter2)
;  (let ((split-point (make-vect 0.0 0.5)))
;    (let ((paint-bottom (transform-painter painter1
;                                           (make-vect 0.0 0.0)
;                                           (make-vect 1.0 0.0)
;                                           split-point))
;          (paint-top    (transform-painter painter2
;                                           split-point
;                                           (make-vect 1.0 0.0)
;                                           (make-vect 0.0 1.0))))
;      (lambda (frame)
;        (paint-bottom frame)
;        (paint-top    frame)))))

;(define (below paint1 paint2)
;  (rotate270 (beside (rotate90 painter2)
;                     (rotate90 painter1))))

;; exercise 2.52
;a)
(define (wave frame)
  (let ((line1 (list (make-segment (make-vect 0.0 0.8)
                                   (make-vect 0.15 0.6))
                     (make-segment (make-vect 0.15 0.6)
                                   (make-vect 0.3 0.65))
                     (make-segment (make-vect 0.3 0.65)
                                   (make-vect 0.45 0.65))
                     (make-segment (make-vect 0.45 0.65)
                                   (make-vect 0.3 0.8))
                     (make-segment (make-vect 0.3 0.8)
                                   (make-vect 0.45 1.0))))
        (line2 (list (make-segment (make-vect 0.55 1.0)
                                   (make-vect 0.6 0.8))
                     (make-segment (make-vect 0.6 0.8)
                                   (make-vect 0.55 0.65))
                     (make-segment (make-vect 0.55 0.65)
                                   (make-vect 0.6 0.65))
                     (make-segment (make-vect 0.6 0.65)
                                   (make-vect 1.0 0.35))))
        (line3 (list (make-segment (make-vect 1.0 0.2)
                                   (make-vect 0.55 0.5))
                     (make-segment (make-vect 0.55 0.5)
                                   (make-vect 0.6 0.0))))
        (line4 (list (make-segment (make-vect 0.45 0.0)
                                   (make-vect 0.5 0.3))
                     (make-segment (make-vect 0.5 0.3)
                                   (make-vect 0.55 0.0))))
        (line5 (list (make-segment (make-vect 0.0 0.65)
                                   (make-vect 0.15 0.5))
                     (make-segment (make-vect 0.15 0.5)
                                   (make-vect 0.3 0.6))
                     (make-segment (make-vect 0.3 0.6)
                                   (make-vect 0.45 0.55))
                     (make-segment (make-vect 0.45 0.55)
                                   (make-vect 0.15 0.0))))
        (line6 (list (make-segment (make-vect 0.3 0.9)
                                   (make-vect 0.575 0.9)))))
    ((segments->painter (append line1 line2 line3 line4 line5 line6))
     frame)))

;b)
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((right (right-split painter (- n 1)))
            (up (up-split painter (- n 1))))
        (below (beside painter
                       (below right right))
               (beside (beside up up)
                       (corner-split painter (- n 1)))))))

;c)
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-vert rotate180
                                  identity  flip-horiz)))
    (combine4 (corner-split painter n))))
