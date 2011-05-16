; native scheme

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 12)

(define (list . x)
  x)

(list 1 2 3 'a 'b 'c)

(define (reduce proc x)
  (cond ((null? x) false)
	((null? (cdr x)) (car x))
	(else (proc (car x) (reduce proc (cdr x))))))
     
(reduce + '(1 2 3 4 5 6))

(define (dot x . y)
  (+ x (reduce * y)))

(dot 1000 1 2 3 4 5 6)

(define (dec x)
  (if (= x 0)
      0
      (dec (- x 1))))

(dec 500)

(driver-loop) ; execute compiled scheme evaluator

(let ((a 5) (b 6))
  (+ a b))

(define (factorial x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

(factorial 6)

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fib 10)
(fib 11)
(fib 12)

(define (double x)
  (* 2 x))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= b 0) 0)
	((even? b) (double (mul a (halve b))))
	(else (+ a (mul a (- b 1))))))

(mul 25 89)

(mul 3 4)

(cond (123))

(define (map proc x)
  (if (null? x)
      x
      (cons (proc (car x))
	    (map proc (cdr x)))))

(define (square x) (* x x))

(map square '(1 2 3 4 5))

(quit)
