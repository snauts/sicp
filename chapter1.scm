;;;;;;;;;;;;;;;;;
;;; Exercise 1.1
;;;;;;;;;;;;;;;;;

10
;Value: 10

(+ 5 3 4)
;Value: 12

(- 9 1)
;Value: 8

(/ 6 2)
;Value: 3

(+ (* 2 4) (- 4 6))
;Value: 6

(define a 3)
;Value: a

(define b (+ a 1))
;Value: b

(+ a b (* a b))
;Value: 19

(= a b)
;Value: #f

(if (and (> b a) (< b (* a b)))
    b
    a)
;Value: 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
;Value: 16

(+ 2 (if (> b a) b a))
;Value: 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
;Value: 16

;;;;;;;;;;;;;;;;;
;;; Exercise 1.2
;;;;;;;;;;;;;;;;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
   (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.3
;;;;;;;;;;;;;;;;;

(define (square x)
  (* x x))

(define (max x y)
  (if (> x y) x y))

(define (min x y)
  (if (< x y) x y))

(define (largest x y z)
  (max x (max y z)))

(define (second-largest x y z)
  (min (max x y) (min (max x z) (max y z))))

(define (sum-of-squares-of-two-largest x y z)
  (+ (square (largest x y z))
     (square (second-largest x y z))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.4
;;;;;;;;;;;;;;;;;

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; is the same as
(define (a-plus-abs-b-2 a b)
  (if (> b 0) 
      (+ a b)
      (- a b)))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.5
;;;;;;;;;;;;;;;;;
;; normal-order evaluation will return 0
;; applicative-order evaluation will loop infinitely

;;;;;;;;;;;;;;;;;
;;; Exercise 1.6
;;;;;;;;;;;;;;;;;
;; infinite loop happens
;; because new-if evaluates both (then-clause and else-clause) at the same time

;;;;;;;;;;;;;;;;;
;;; Exercise 1.7
;;;;;;;;;;;;;;;;;

(define (abs a)
  (if (> a 0) a (- a)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (my-sqrt x)
  (sqrt-iter 1.0 x))

; if the number is too big (sqrt ...) will go into infinite loop
; because it will not be able to achieve 0.001 precision 
; due to floating point pecularities

; on the other hand for numbers smaller than 0.001 algorithm will claim
; that answer ~0.001 is good enough which is obviously wrong

(define (good-enough-2-? guess old-guess)
  (< (abs (- 1.0 (/ old-guess guess))) 0.001))

(define (sqrt-iter-2 guess old-guess x)
  (if (good-enough-2-? guess old-guess)
      guess
      (sqrt-iter-2 (improve guess x) guess x)))

(define (sqrt-2 x)
  (sqrt-iter-2 1.0 0.0 x))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.8
;;;;;;;;;;;;;;;;;

(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

(define (cube-root-iter guess old-guess x)
  (if (good-enough-2-? guess old-guess)
      guess
      (cube-root-iter (improve-cube guess x) guess x)))

(define (cube-root x)
  (cube-root-iter 1.0 0.0 x))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.9
;;;;;;;;;;;;;;;;;

;recursive
;(define (+ a b)
;  (if (= a 0)
;      b
;      (inc (+ (dec a) b))))

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

;iterative
;(define (+ a b)
;  (if (= a 0)
;      b
;      (+ (dec a) (inc b))))

;(+ 4 5)
;(+ 3 6)
;(+ 2 7)
;(+ 1 8)
;(+ 0 9)
;9

;;;;;;;;;;;;;;;;;
;;; Exercise 1.10
;;;;;;;;;;;;;;;;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
;Value: 1024

(A 2 4)
;Value: 65536

(A 3 3)
;Value: 65536

(define (f n) (A 0 n))
; 2 * n

(define (g n) (A 1 n))
; (A 1 1) = 2
; (A 1 2) = (A 0 (A 1 1)) = (A 0 2) = 4
; (A 1 3) = (A 0 (A 1 2)) = (A 0 4) = 8
; (A 1 4) = (A 0 (A 1 3)) = (A 0 8) = 16
; n ^ 2

(define (h n) (A 2 n))
; (A 2 1) = 2
; (A 2 2) (A 1 (A 2 1)) = (A 1 2)  = 2^2
; (A 2 3) (A 1 (A 2 2)) = (A 1 2^2)  = 2^2^2
; (A 2 4) (A 1 (A 2 3)) = (A 1 2^2^2) = 2^2^2^2

; n=1 2
; n=2 2^2
; n=3 2^2^2
; n=4 2^2^2^2
; n=5 2^2^2^2^2

(define (k n) (* 5 n n))
; 5 * n^2

;;;;;;;;;;;;;;;;;
;;; Exercise 1.11
;;;;;;;;;;;;;;;;;

(define (f n)
  (cond ((< n 3) n)
	(else (+ (f (- n 1)) 
		 (* 2 (f (- n 2)))
		 (* 3 (f (- n 3)))))))

(define (f-iterative n)
  (define (fi f-1 f-2 f-3 n)
    (if (= n 0)
	f-1
	(fi (+ f-1 (* 2 f-2) (* 3 f-3)) f-1 f-2 (- n 1))))
  (cond ((< n 3) n)
	(else (fi 2 1 0 (- n 2)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.12
;;;;;;;;;;;;;;;;;

(define (pascal row num)
  (cond ((= num 0) 1)
	((= num row) 1)
	(else (+ (pascal (- row 1) num)
		 (pascal (- row 1) (- num 1))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.13
;;;;;;;;;;;;;;;;;

; (fib 0) = 0, (fi^n)/sqrt(5) = 0.44
; (fib 1) = 1, (fi^n)/sqrt(5) = 0.72
; (fib 2) = 1, (fi^n)/sqrt(5) = 1.17
; (fib 3) = 2, (fi^n)/sqrt(5) = 1.89
; (fib 4) = 3, (fi^n)/sqrt(5) = 3.06
; (fib 5) = 5, (fi^n)/sqrt(5) = 4.95
; (fib 6) = 8, (fi^n)/sqrt(5) = 8.02

; (fib (- n 1))	~ (fi^(n-1))/sqrt(5)
; (fib n)	~ (fi^n)/sqrt(5)

; (fib (+ n 1)) = (+ (fib n) (fib (- n 1))) 
; ~
; (fi^(n-1))/sqrt(5) + (fi^n)/sqrt(5)
; (fi^(n-1) + fi^n)/sqrt(5)
; (fi^(n-1) * (1 + fi))/sqrt(5)
; by golden mean definition fi^2 = fi + 1
; substuting (fi^(n-1) * fi^2) / sqrt(5)
; fi^(n+1) / sqrt(5) Q.E.D.

; THIS IS WRONG

;;;;;;;;;;;;;;;;;
;;; Exercise 1.14
;;;;;;;;;;;;;;;;;

;                      11
;                     /  \
;		     /    \
;                   1      11
;                         /  \
;                        /    \
;                       6      11
;                      / \     /
;                     1   6  ...
;                        /   / 
;                      ...  1
;                      /
;                     1

; count-change has time O(amount^kinds-of-coins) in this particular case O(n^5)
; count-change has space O(amount+kinds-of-coins)

;;;;;;;;;;;;;;;;;
;;; Exercise 1.15
;;;;;;;;;;;;;;;;;

; a) p is applied 5 times
; b) both time and space is O(log(n))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.16
;;;;;;;;;;;;;;;;;

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (fast-expt-iter-acc n a x)
    (cond ((= n 0) a)
	  ((even? n) (fast-expt-iter-acc (/ n 2) a (* x x)))
	  (else (fast-expt-iter-acc (- n 1) (* a x) x))))
  (fast-expt-iter-acc n 1 b))

(define (test-expt)
  (define (dump-data base n a b)
    (display "base=")
    (print base)
    (display " n=")
    (print n)
    (display " ret1=")
    (print a)
    (display " ret2=")
    (print b)
    (newline))
  (define (debug-test base n a b)
    (if (not (= a b))
	(dump-data base n a b)
	#t))
  (define (test-base b n)
    (debug-test b n (fast-expt b n) (fast-expt-iter b n))
    (if (> n 0)
	(test-base b (- n 1))
	#t))
  (define (do-tests base)
    (if (> base 1)
	(begin
	  (test-base base 100)
	  (do-tests (- base 1)))
	#t))
  (do-tests 100)
  (display "ok")
  (newline))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.17
;;;;;;;;;;;;;;;;;

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= b 0) 0)
	((even? b) (double (mul a (halve b))))
	(else (+ a (mul a (- b 1))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.18
;;;;;;;;;;;;;;;;;

(define (mul-iter a b)
  (define (mul-iter-acc a n x)
    (cond ((= n 0) x)
	  ((even? n) (mul-iter-acc (double a) (halve n) x))
	  (else (mul-iter-acc a (- n 1) (+ x a)))))
  (mul-iter-acc a b 0))
  
;;;;;;;;;;;;;;;;;
;;; Exercise 1.19
;;;;;;;;;;;;;;;;;

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
		   (+ (* p p) (* q q))
		   (+ (* p q) (* q q) (* q p))
		   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(define (debug-fib n)
  (define (debug-fib-2 i)
    (if (not (= n i))
	(begin
	  (print (fib i))
	  (display " ")
	  (debug-fib-2 (+ i 1)))
	#t))
  (debug-fib-2 0)
  (newline))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.20
;;;;;;;;;;;;;;;;;

(define *counter* 0)

(define (counted-remainder a b)
  (set! *counter* (+ *counter* 1))
  (remainder a b))

(define (gcd a b)
  (set! *counter* 0)
  (display "result=")
  (print (gcd-real a b))
  (newline)
  (display "counter=")
  (print *counter*)
  (newline))

(define (gcd-real a b)  
  (if (= b 0)
      a
      (gcd-real b (counted-remainder a b))))

; 4 remainder operations

(define (get x)
  (if (number? x) x (x)))

(define (gcd-real a b)
  (if (= (get b) 0)
      (get a)
      (gcd-real b (lambda () (counted-remainder (get a) (get b))))))

; 18 remainder operations

;;;;;;;;;;;;;;;;;
;;; Exercise 1.21
;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (prime? n)
  (= n (smallest-divisor n)))

; 199 -> 199
; 1999 -> 1999
; 19999 -> 7

;;;;;;;;;;;;;;;;;
;;; Exercise 1.22
;;;;;;;;;;;;;;;;;

(define (timed-prime-test n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime n (- (runtime) start-time))
      #f))
(define (report-prime n elapsed-time)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline)
  #t)

(define (search-for-primes n count)
  (cond ((= count 0) #t)
	(else (if (timed-prime-test n)
		  (search-for-primes (+ n 1) (- count 1))
		  (search-for-primes (+ n 1) count)))))

; sqrt(10) ~ 3.1

;1 ]=> (search-for-primes 1000000000 3)
;1000000007 *** 0.06
;1000000009 *** 0.07
;1000000021 *** 0.07
;Value: #t

;1 ]=> (search-for-primes 10000000000 3)	; 0.07 * 3.1 ~ 0.21
;10000000019 *** 0.21
;10000000033 *** 0.21
;10000000061 *** 0.22 
;Value: #t 

;1 ]=> (search-for-primes 100000000000 3)	; 0.21 * 3.1 ~ 0.65
;100000000003 *** 0.67
;100000000019 *** 0.67
;100000000057 *** 0.70
;Value: #t

;1 ]=> (search-for-primes 1000000000000 3)	; 0.67 * 3.1 ~ 2.07
;1000000000039 *** 2.22
;1000000000061 *** 2.20
;1000000000063 *** 2.20
;Value: #t

;1 ]=> (search-for-primes 10000000000000 3)	; 2.20 * 3.1 ~ 6.82
;10000000000037 *** 6.97
;10000000000051 *** 6.93
;10000000000099 *** 6.91
;Value: #t

;;;;;;;;;;;;;;;;;
;;; Exercise 1.23
;;;;;;;;;;;;;;;;;

(define (next x)
  (cond ((= x 2) 3)
	(else (+ x 2))))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;1 ]=> (search-for-primes 1000000000000 3)	; (/ 2.21 1.32) = 1.67
;1000000000039 *** 1.32
;1000000000061 *** 1.32
;1000000000063 *** 1.31
;Value: #t

;1 ]=> (search-for-primes 10000000000000 3)	; (/ 6.95 4.26) = 1.63
;10000000000037 *** 4.26
;10000000000051 *** 4.25
;10000000000099 *** 4.26
;Value: #t

; because 1/6 numbers fail primality test when divided by 3, 
; when this optimization does not kick in yet 
; intersetingly 2 - 1/6 ~ 1.666

;;;;;;;;;;;;;;;;;
;;; Exercise 1.24
;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10000)
      (report-prime n (- (runtime) start-time))
      #f))

;1 ]=> (search-for-primes 100000000000 3) 
;100000000003 *** 1.40
;100000000019 *** 1.44
;100000000057 *** 1.47

; (/ (log 100000000000) (log 5e7)) ~ 1.42

;1 ]=> (search-for-primes 1000000000000 3)
;1000000000039 *** 1.50
;1000000000061 *** 1.53
;1000000000063 *** 1.56

; (/ (log 1000000000000) (log 5e7)) ~ 1.55

;1 ]=> (search-for-primes 10000000000000 3)
;10000000000037 *** 1.61
;10000000000051 *** 1.64
;10000000000099 *** 1.66

; (/ (log 10000000000000) (log 5e7)) ~ 1.68

; fuck yeah

;;;;;;;;;;;;;;;;;
;;; Exercise 1.25
;;;;;;;;;;;;;;;;;

; this monstrosity will use terribly big bignums so multiplying those 
; and storing those can not be considered constant any more
; bignum n will contain O(log(n)) space and would take O(log(n)) steps to
; multiply it, so fast-exp which is O(log(n)) when multiplication is considered
; to perform in constant time, with big nums will be O(log(n)^2) steps

; THIS IS WRONG

;;;;;;;;;;;;;;;;;
;;; Exercise 1.26
;;;;;;;;;;;;;;;;;

; let's take scenario where exp = 2^n
; original expmod would always take (even? exp) branch and have n steps
; louises expmod always takes (even? exp) branch as well, but at every
; iteration it make two identical sub-calls resulting in 2^n steps

; THIS IS WRONG
; O(n)

;;;;;;;;;;;;;;;;;
;;; Exercise 1.27
;;;;;;;;;;;;;;;;;

(define (full-brute-fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test x)
    (cond ((= x n) #t)
	  ((not (try-it x)) #f)
	  (else (test (+ x 1)))))
  (test 2))

(full-brute-fermat-test 561)	; #t
(full-brute-fermat-test 1105)	; #t
(full-brute-fermat-test 1729)	; #t
(full-brute-fermat-test 2465)	; #t
(full-brute-fermat-test 2821)	; #t
(full-brute-fermat-test 6601)	; #t

;;;;;;;;;;;;;;;;;
;;; Exercise 1.28
;;;;;;;;;;;;;;;;;

(define (m-r-square x m)
  (define (is-still-prime? s)
    (not (and (not (= x 1)) (not (= x (- m 1))) (= 1 (remainder s m)))))
  (define (square-or-bust s)
    (if (is-still-prime? s) s 0))
  (square-or-bust (square x)))

(define (m-r-expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
	 (remainder (m-r-square (m-r-expmod base (/ exp 2) m) m) m))
        (else (remainder (* base (m-r-expmod base (- exp 1) m)) m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (m-r-expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (test-miller-rabin x y)
  (cond ((not (eq? (fast-prime? x 100) (prime? x)))
	 (display " ")
	 (display y))
	(else #t))
  (if (> x y)
      (display "ok")
      (test-miller-rabin x (- y 1))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.29
;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (identity x) x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (simpson-rule f a b n)  
  (simpson-rule-real f a b (+ n (if (even? n) 0 1))))

(define (simpson-rule-real f a b n)  
  (define (local h) 
    (define (mul k)
      (cond ((= k 0) 1)
	    ((= k n) 1)
	    ((even? k) 2)
	    (else 4)))
    (define (y k)
      (* (mul k) (f (+ a (* k h)))))
    (* (/ h 3) (sum y 0 inc n)))
  (local (/ (- b a) n)))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.30
;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.31
;;;;;;;;;;;;;;;;;

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (my-pi n)
  (define (pi-next x)
    (+ x 2))
  (define (pi-term x)
    (/ (* x (pi-next x)) (square (+ x 1.0))))
  (* 4 (product pi-term 2 pi-next n)))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.32
;;;;;;;;;;;;;;;;;

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner 
       (term a)
       (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
   (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.33
;;;;;;;;;;;;;;;;;

(define (filtered-accumulate should-use? combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (if (should-use? a)
			   (combiner result (term a))
			   result))))
  (iter a null-value))

(define (sum-squares-of-primes a b)
  (filtered-accumulate prime? + 0 square a inc b))


(define (exercise-1.33-b n)
  (define (gcd a b)  
    (if (= b 0) a (gcd b (remainder a b))))
  (define (gcd-is-1? i)
    (= (gcd i n) 1))
  (filtered-accumulate gcd-is-1? * 1 identity 1 inc n))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.34
;;;;;;;;;;;;;;;;;

; (define (f g) (g 2))
; (f f) -> (f 2) -> (2 2)
; trying to use 2 as a function will fail

;;;;;;;;;;;;;;;;;
;;; Exercise 1.35
;;;;;;;;;;;;;;;;;

(define tolerance 0.00001)

(define *guess* 0)
(define *count* 0)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))      
      (set! *count* (inc *count*))
      (if (cond ((= *count* 1000) (set! *guess* next) #t)
		((> *count* 1000) (if (= *guess* next)
				      (begin (display "bad ") #f)
				      #t))
		(else #t))
	  (if (close-enough? guess next)
	      next
	      (try next))
	  #f)))
  (set! *count* 0)
  (try first-guess))

(define (average-damp f)
  (lambda (x) (average x (f x))))

;; x -> 1 + 1/x
;; multiply both sides with x
;; x^2 -> x + 1 ; the golden mean definition

(define (golden-mean)
  (fixed-point (lambda (x) (+ 1 (/ 1 x)))
	       1.0))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.36
;;;;;;;;;;;;;;;;;

; 35 iterations
(define (exercise-1.36)
  (fixed-point (lambda (x) (/ (log 1000) (log x)))
	       2.0))

; 10 iterations
(define (exercise-1.36-damping)
  (fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
	       2.0))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.37
;;;;;;;;;;;;;;;;;

; 0.61803

(define (cont-frac-recursive n d k)
  (define (local i)
    (if (< k i)
	0
	(/ (n i) (+ (d i) (local (+ i 1))))))
  (local 1))

; k must be atleast 11 to get precision of 4 decimal places
(define (golden-mean)
  (cont-frac-iterative (lambda (i) 1.0) (lambda (i) 1.0) 11))

(define (cont-frac-iterative n d k)
  (define (local result i)
    (if (= i 0)
	result
	(local (/ (n i) (+ (d i) result)) (- i 1))))
  (local 0 k))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.38
;;;;;;;;;;;;;;;;;

(define (euler k)
  (define (d x)
    (let ((x (- x 2)))
      (if (= (remainder x 3) 0)
	  (* 2 (+ (/ x 3) 1))
	  1)))
  (+ 2 (cont-frac-iterative (lambda (i) 1.0) d k)))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.39
;;;;;;;;;;;;;;;;;

(define (tan-cf x k)
  (cont-frac-iterative
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (- (* i 2) 1))
   k))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.40
;;;;;;;;;;;;;;;;;

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.41
;;;;;;;;;;;;;;;;;

(define (double f)
  (lambda (x) (f (f x))))

; (((double (double double)) inc) 5) ; returns 21

;;;;;;;;;;;;;;;;;
;;; Exercise 1.42
;;;;;;;;;;;;;;;;;

(define (compose f g)
  (lambda (x) (f (g x))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.43
;;;;;;;;;;;;;;;;;

(define (repeated f n)
  (define (repeated-fn)
    (if (= n 1)
	f
	(compose f (repeated f (- n 1)))))
  (lambda (x) ((repeated-fn) x)))

(define (repeated f n) ; less complicated
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.44
;;;;;;;;;;;;;;;;;

(define dx 0.00001)

(define (smooth f) ; wrong
  (lambda (x) (* 1/3 (f x) (f (- x dx)) (f (+ x dx)))))

(define (smooth f) ; right
  (lambda (x) (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)))

(define (n-smooth f n) ; wrong
  (repeated (smooth f) n))

(define (n-smooth f n) ; right
  ((repeated smooth n) f))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.45
;;;;;;;;;;;;;;;;;

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (newton-root x n)
  (newtons-method (lambda (y) (- (expt y n) x)) 1.0))

(define (damped-root x n)
  (define (fn y) (/ x (expt y (- n 1))))
  (define (damp-count) (floor (/ (log n) (log 2))))
  (display "damp count = ")
  (display (damp-count))
  (newline)
  (fixed-point ((repeated average-damp (damp-count)) fn) 1.0))

;;;;;;;;;;;;;;;;;
;;; Exercise 1.46
;;;;;;;;;;;;;;;;;

(define (iterative-improve good-enough? improve)
  (define (ii guess)
    (if (good-enough? guess)
	guess
	(ii (improve guess))))
  (lambda (guess) (ii guess)))

(define (iterative-improve good-enough? improve) ; less complicated
  (define (ii guess)
    (if (good-enough? guess)
	guess
	(ii (improve guess))))
  ii)
    
(define (ii-sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve) x))

(define (ii-fixed-point f x)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) 0.000001))
  ((iterative-improve close-enough? f) x))
