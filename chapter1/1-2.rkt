#lang sicp

(#%provide (all-defined))

; load from 1.1
(define (square x) (* x x))

;; exercise 1.9
(define (plus-rec a b)
  (if (= a 0)
      b
      (inc (plus-rec (dec a) b))))
;(+ 4 5)
;(inc (+ 3 5))
;(inc (inc (+ 2 5)))
;(inc (inc (inc (+ 1 5))))
;(inc (inc (inc (inc (+ 0 5)))))
;(inc (inc (inc (inc 5))))
;(inc (inc (inc 6)))
;(inc (inc 7))
;(inc 8)
;9

(define (plus-iter a b)
  (if (= a 0)
      b
      (plus-iter (dec a) (inc b))))
;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9


;; exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; (A 1 10)
; = 1024
; (A 2 4)
; = 65536
; (A 3 3)
; = 65536

(define (f n) (A 0 n))
; f(n) = 2n
(define (g n) (A 1 n))
; g(n) = 2^n
(define (h n) (A 2 n))
; h(n) = 2^h(n-1)
; h(1) = 2


;; exercise 1.11
(define (tribo-rec n)
  (if (< n 3)
      n
      (+ (tribo-rec (- n 1))
         (tribo-rec (- n 2))
         (tribo-rec (- n 3)))))

(define (tribo-iter a b c n)
  (cond ((= n 0) a)
        ((= n 1) b)
        ((= n 2) c)
        (else (tribo-iter b
                          c
                          (+ a
                             (* 2 b)
                             (* 3 c))))))

(define (tribo n)
  (tribo-iter 0 1 2 n))

;; exercise 1.12
(define (pascal r c)
  (if (or (= r 1) (= r c))
      1
      (+ (pascal (- r 1) c)
         (pascal (- r 1) (- c 1)))))

;; exercise 1.13

;; exercise 1.14

;; exercise 1.15
;; a)

;; (sine 12.15)를 계산하면 다음과 같은 과정을 거친다.
;(sine 12.15)
;(sine 4.05)
;(sine 1.35)
;(sine 0.45)
;(sine 0.15)
;(sine 0.05)
;; 따라서 (sine 12.15)를 계산할 때 p를 5번 계산한다.

;; b)
; f(n) = log(n) + log(10)/log(3)

;; exercise 1.16
(define (fast-expt-iter b n res)
  (cond ((= n 0) res)
        ((even? n) (fast-expt-iter (square b) (/ n 2) res))
        (else (fast-expt b (- n 1) (* res b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))

;; exercise 1.17 & 1.18

; recursive
;(define (* a b)
;  (cond ((= b 0) 0)
;        ((even? b) (double (* a (halve b))))
;        (else (+ a (* a (- b 1))))))

; iterative
;(define (*-iter a b res)
;  (cond ((= b 0) res)
;        ((even? b) (*-iter (double a) (halve b) res))
;        (else (*-iter a (- b 1) (+ res a)))))

;; exercise 1.19
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; 1.2.5
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;; exercise 1.20

; 정의대로 계산할 떄 a와 b에 remainder가 계속 누적되고
; 이는 gcd를 한번 계산할 때마다 그 식을 계산하게 된다.
; 따라서 유클리드 알고리즘을 통해 k단계를 거쳤다면
; f(0)=0, f(1)=1, f(n)=f(n-1)+f(n-2)+1 일때
; remainder를 2f(n) + f(n-1)번 계산한다.

; (gcd 206 40)
; (gcd 40 (remainder 206 40)) -> 0번
; (gcd (remainder 206 40) (remainder 40 (remainder 206 40))) -> remainder 1번
; (gcd (remainder 40 (remainder 206 40)) -> 2번
;      (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) -> 4번
;      (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; 2 -> 7 + 4번
; => 총 18번

; 인자먼저 계산하면 다음 식으로 중간 계산값을 넘길 때
; b에 들어가는 remainder가 계산되므로
; gcd를 계속 계산한다고 해도 각 단계마다
; remainder를 한번만 계산한다.
; 따라서 유클리드 알고리즘을 통해 k단계를 거쳤다면
; remainder를 k번 계산한다.

; (gcd 206 40)
; (gcd 40 6) -> remainder 1번
; (gcd 6 4) -> remainder 1번
; (gcd 4 2) -> remainder 1번
; (gcd 2 0) -> remainder 1번
; 2
; => 총 4번

;; 1.2.6
(define (smallest-divisor n)
  (find-divisor n 2))

; commented for 1.23
;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; commented for 1.28
;(define (expmod base exp m)
;  (cond ((= exp 0) 1)
;        ((even? exp)
;         (remainder (square (expmod base (/ exp 2) m))
;                    m))
;        (else
;         (remainder (* base (expmod base (- exp 1) m))
;                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; exercise 1.21
; > (smallest-divisor 199)
; 199
; > (smallest-divisor 1999)
; 1999
; > (smallest-divisor 19999)
; 7

;; exercise 1.22

; 결과량을 줄이기 위해
; timed-prime-test와 report-prime을 수정함
(define (timed-prime-test n)
  (start-prime-test n (runtime)))

; commented for 1.24
;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes start end)
  (define (search-n-back n)
    (timed-prime-test n)
    (search-for-primes (+ n 1) end))
  (if (or (< start end)
          (= start end))
    (search-n-back start)))

;1009 *** 0
;1013 *** 0
;1019 *** 0

;10007 *** 0
;10009 *** 0
;10037 *** 0

;100003 *** 0
;100019 *** 0
;100043 *** 0

;1000003 *** 0
;1000033 *** 0
;1000037 *** 0

; 대부분 소수인지 판별하는데 1s미만이라 10^1/2인지 확인할 수 없음
; 다만 어떤 소수는 1000이 나오기도 하며 이는 일정치 않음
; 그렇기에 항상 시간이 O(n^1/2)를 완전히 따른다고 할 수 없음.

;; exercise 1.23
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

; 이 문제 또한 각 소수들을 구하는 시간이 너무 짧아
; 정확하게 측정하기는 어렵다.

;; exercise 1.24

(define (start-prime-test n start-time)
  (if (fast-prime? n 3)
      (report-prime n (- (runtime) start-time))))

; 전부 0으로 나와 측정 불가

;; exercise 1.25
; 만약 이 언어가 표현할 수 있는 정수에 한계가 있다면
; 1.25에서 정의한 방법대로 계산할 수 없다.
; 특히 지수 계산이기 때문에
; 조금만 수를 올려도 한계치에 도달할 수 있다.
; 그렇기 때문에 계산할 값의 크기를 줄여
; 이런 문제를 예방하는 방법이 더 좋다.

;; exercise 1.26
; 이 프로시저는 exp가 짝수인 경우 (/ exp 2)식을 통해
; 계산할 때 필요한 exp 수를 n에서 log_2(n)으로 줄였다.
; 하지만 이러한 재귀식을 2번쓰게되면
; 같은 계산을 2번하게되므로
; n에서 2^n으로 expmod의 호출량이 많아지게 된다.
; 결국 Loise가 쓴 정의대로 프로시저를 만들면
; 계산하는 시간이 O(n)을 따라간다.

;; exercise 1.27
(define (test-fermat n)
  (define (modulo-check a n)
    (= (expmod a n n) a))
  (define (modulo-filter a n)
    (if (not (modulo-check a n))
        (display-number a)))
  (define (display-number n)
    (display n)
    (newline))
  (define (iter i a)
    (if (< i a)
        (run-and-back i a)))
  (define (run-and-back i a)
    (modulo-filter i n)
    (iter (+ i 1) a))
  (iter 1 n))

; test-fermat은 페르마 검사에서 조건을 만족하지 않는 a값을 보여준다.
; 즉, 결과가 하나라도 나온다면 합성수
; 결과가 나오지 않는다면 소수여야 한다. 하지만,

; > (smallest-divisor 561)
; 3
; > (test-fermat 561)
; >

; > (smallest-divisor 1105)
; 5
; > (test-fermat 1105)
; >

; > (smallest-divisor 1729)
; 7
; > (test-fermat 1729)
; >

; > (smallest-divisor 2465)
; 5
; > (test-fermat 2465)
; >

; > (smallest-divisor 2821)
; 7
; > (test-fermat 2821)
; >

; > (smallest-divisor 6601)
; 7
; > (test-fermat 6601)
; >

; 각주 47에서 나온 카마이클 수 561, 1105, 2465, 2821, 6601은
; 엄연히 합성수임에도 페르마 검사를 만족한다.

;; exercise 1.28
(define (expmod base exp m)
  (cond ((or (= base 1)
             (= base (- exp 1)))
         0)
        ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (ml-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

; 위의 코드는 fermat-test를 변형해서 작성했다.
; 밀러-라빈 검사의 조건을 이용하여 확실하게 검사하도록 짠다면,

(define (ml-prime? n)
  (define (limit-of-test)
    (+ (/ (- n (remainder n 2)) 2) 2))
  (define (ml-check a)
    (define (try-it a)
      (not (= (expmod a (- n 1) n) 1)))
    (try-it a))
  (define (iter i limit)
    (cond ((> i limit) #f)
          ((ml-check i) (iter (+ i 1) limit))
          (else #t)))
  (iter 2 (limit-of-test)))
