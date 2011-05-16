;;;;;;;;;;;;;;;;;
;;; Exercise 3.1
;;;;;;;;;;;;;;;;;

(define (make-accumulator sum)
  (lambda (amount)
    (set! sum (+ sum amount))
    sum))

(define (test-3.1)
  (define accumulator (make-accumulator 10))
  (assert (lambda () (accumulator 10)) 20)
  (assert (lambda () (accumulator 50)) 70)
  (assert '((make-accumulator 30) 30) 60))

(test-3.1)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.2
;;;;;;;;;;;;;;;;;

(define (make-monitored f)
  (let ((count 0))
    (lambda (arg)
      (cond ((eq? arg 'how-many-calls?) count)
	    ((eq? arg 'reset-count) (set! count 0))
	    (else (set! count (inc count))
		  (f arg))))))

(define (test-3.2)
  (define monitored-sqrt (make-monitored sqrt))
  (assert (lambda () (monitored-sqrt 100)) 10)
  (assert (lambda () (monitored-sqrt 'how-many-calls?)) 1)
  (monitored-sqrt 'reset-count)
  (assert (lambda () (monitored-sqrt 'how-many-calls?)) 0)
  (assert (lambda () (monitored-sqrt 36)) 6)
  (assert (lambda () (monitored-sqrt 49)) 7)
  (assert (lambda () (monitored-sqrt 64)) 8)
  (assert (lambda () (monitored-sqrt 'how-many-calls?)) 3))

(test-3.2)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.3
;;;;;;;;;;;;;;;;;

(define (make-account balance secret-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (check-password password)
    (eq? password secret-password))
  (define (dispatch password m)
    (cond  ((eq? m 'check-password) check-password)
	   ((not (eq? password secret-password))
	    (lambda x "Incorrect password"))	   
	   ((eq? m 'withdraw) withdraw)
	   ((eq? m 'deposit) deposit)
	   (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)

(define ex-3.3-make-account make-account)

(define (test-3.3)
  (define acc (make-account 100 'secret-password))
  (define (test1) ((acc 'secret-password 'withdraw) 40))
  (define (test2) ((acc 'some-other-password 'deposit) 50))
  (assert test1 60)
  (assert test2 "Incorrect password"))

(test-3.3)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.4
;;;;;;;;;;;;;;;;;

(define max-fails 7)

(define (call-the-cops)
  (lambda x "You are busted"))

(define (make-account balance secret-password)
  (let ((fails 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (login-ok m)
      (set! fails 0)
      (cond  ((eq? m 'withdraw) withdraw)
	     ((eq? m 'deposit) deposit)
	     (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    (define (dispatch password m)
      (if (eq? password secret-password)
	  (login-ok m)
	  (begin
	    (set! fails (inc fails))
	    (if (>= fails max-fails)
		(call-the-cops) 
		(lambda x "Incorrect password")))))
    dispatch))

(define (test-3.4)
  (define acc (make-account 100 'secret-password))
  (define (bad-op) ((acc 'some-other-password 'withdraw) 10))
  (define (good-op) ((acc 'secret-password 'deposit) 10))
  (define (repeat x)
    (define (repeating y)
      (assert bad-op
	      (if (>= y max-fails)
		  "You are busted"
		  "Incorrect password"))
      (if (>= y x) #f (repeating (inc y))))
    (repeating 1))
  (repeat (floor (/ max-fails 2)))
  (assert good-op 110)
  (repeat max-fails))

(test-3.4)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.5
;;;;;;;;;;;;;;;;;

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (make-circle cx cy r)
  (lambda (x y)
    (<= (+ (square (- x cx))
	   (square (- y cy)))
	(square r))))

(define (surface x1 y1 x2 y2)
  (* (abs (- x2 x1)) (abs (- y2 y1))))

(define (random-in-range a b)
  (let ((range (abs (- a b))))
    (+ (min a b) (float-random range))))

(define (test-integral predicate x1 y1 x2 y2)
  (lambda ()
    (predicate 
     (random-in-range x1 x2)
     (random-in-range y1 y2))))

(define (estimate-integral predicate x1 y1 x2 y2 trials)
  (* (surface x1 y1 x2 y2)
     (monte-carlo trials (test-integral predicate x1 y1 x2 y2))))

(define (test-3.5)
  (assert
   '(estimate-integral (make-circle 0 0 1) -1.0 -1.0 1.0 1.0 100000)
   (lambda (x) (< (- pi 0.5) x (+ pi 0.5))))
  (assert
   '(estimate-integral (make-circle 5 7 3) 2.0 4.0 8.0 10.0 100000)
   (lambda (x)
     (define s (* pi (square 3)))
     (< (- s 0.5) x (+ s 0.5)))))
  
(test-3.5)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.6
;;;;;;;;;;;;;;;;;

(define (rand-update x)
  (remainder (+ (* 1103515245 x) 12345) (expt 2 32)))

; note that seed is 32 bit, but random value returned must be 31 bit

(define rand
  (let ((seed 1))
    (lambda (m)
      (cond ((eq? m 'generate) 
	     (set! seed (rand-update seed))
	     (remainder seed (expt 2 31)))
	    ((eq? m 'reset)
	     (lambda (x) (set! seed x)))
	    (else (error "Unknown command -- RAND" m))))))

(define (test-3.6)
  ((rand 'reset) 2)
  (let ((rand-list (map (lambda (x) (rand 'generate)) (make-list 6 0))))
    ((rand 'reset) 2)
    (assert '(map (lambda (x) (rand 'generate)) (make-list 6 0)) rand-list)))

(test-3.6)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.7
;;;;;;;;;;;;;;;;;

(define make-account ex-3.3-make-account)

(define (make-joint account old-password new-password)
  (if (not ((account old-password 'check-password) old-password))
      "Go away, thief!"
      (lambda (password message)
	(if (not (eq? password new-password))
	    "Stop bothering me, thief!"
	    (account old-password message)))))
      
(define (test-3.7)
  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
  (define (test1) (make-joint peter-acc 'lucky-guess 'xxx))
  (define (test2) (paul-acc 'lucky-guess 'withdraw))
  (define (test3) ((peter-acc 'open-sesame 'deposit) 20))
  (define (test4) ((paul-acc 'rosebud 'withdraw) 50))
  (assert test1 "Go away, thief!")
  (assert test2 "Stop bothering me, thief!")
  (assert test3 120)
  (assert test4 70))

(test-3.7)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.8
;;;;;;;;;;;;;;;;;

(define (test-3.8)
  (define f
    (let ((s false))
      (lambda (x)
	(if s
	    x
	    (begin
	      (set! s true)
	      (- 1 x))))))
  (debug "(+ (f 0) (f 1)) = " (+ (f 0) (f 1)) "\n")
  (debug "(+ (f 1) (f 0)) = " (+ (f 1) (f 0)) "\n"))

(test-3.8)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.9
;;;;;;;;;;;;;;;;;

;(define (factorial n)
;  (if (= n 1)
;      1
;      (* n (factorial (- n 1)))))

; (factorial 6) 

; E1: n = 6
; E2: n = 5
; E3: n = 4
; E4: n = 3
; E5: n = 2
; E6: n = 1

; E1 - E6 are all enclosed in global environment

;(define (factorial n)
;  (fact-iter 1 1 n))

;(define (fact-iter product counter max-count)
;  (if (> counter max-count)
;      product
;      (fact-iter (* counter product)
;  		  (+ counter 1)
;		  max-count)))


; E1: n = 6
; E2; product = 1,   counter = 1, max-count = 6
; E3; product = 1,   counter = 2, max-count = 6
; E4; product = 2,   counter = 3, max-count = 6
; E5; product = 6,   counter = 4, max-count = 6
; E6; product = 24,  counter = 5, max-count = 6
; E7; product = 120, counter = 6, max-count = 6
; E8; product = 720, counter = 7, max-count = 6

; E1 - E8 are all enclosed in global environment

;;;;;;;;;;;;;;;;;
;;; Exercise 3.10
;;;;;;;;;;;;;;;;;

;(define (make-withdraw initial-amount)
;  (let ((balance initial-amount))
;    (lambda (amount)
;      (if (>= balance amount)
;	   (begin (set! balance (- balance amount))
;		  balance)
;	   "Insufficient funds"))))

;(define (make-withdraw initial-amount)
;  ((lambda (balance)
;     (lambda (amount)
;       (if (>= balance amount)
;	    (begin (set! balance (- balance amount))
;		   balance)
;	    "Insufficient funds")))
;   initial-amount))

; =============================================================================

; (define W1 (make-withdraw 100))

; +---- global environment----+
; | W1                        |
; +---------------------------+
;               |
;     +--------------------+
; E1: |initial-amount = 100| (make-withdraw 100)
;     +--------------------+
;               |
;     +--------------------+
; E2: |    balance = 100   | ((lambda (balance) ...) 100)
;     +--------------------+
;               |
;            +-+|+
;        W1: |*|*|
;            +|+-+
;             |
;     param: amount
;     code: (if (>= balance amount) ...)

; =============================================================================

; (W1 50)

; +---- global environment----+
; | W1                        |
; +---------------------------+
;               |
;     +--------------------+
; E1: |initial-amount = 100|
;     +--------------------+
;               |
;     +--------------------+
; E2: | balance = 100 - 50 |
;     +--------------------+
;           |        |
;        +-+|+   +-------------+
;    W1: |*|*|   | amount = 50 |
;        +|+-+   +-------------+
;         |
; param: amount
; code: (if (>= balance amount) ...)

; =============================================================================

; (define W2 (make-withdraw 100))

; +---- global environment------------------------------------+
; | W1, W2                                                    |
; +-----------------------------------------------------------+
;               |                              |
;     +--------------------+         +--------------------+
; E1: |initial-amount = 100|     E3: |initial-amount = 100|
;     +--------------------+         +--------------------+
;               |                              |
;     +--------------------+         +--------------------+
; E2: |    balance = 50    |     E4: |    balance = 100   |
;     +--------------------+         +--------------------+
;           |                              |
;        +-+|+                          +-+|+
;    W1: |*|*|                      W2: |*|*|
;        +|+-+                          +|+-+
;         |                              |
; param: amount                          |
; code: (if (>= balance amount) ...) ----+

;;;;;;;;;;;;;;;;;
;;; Exercise 3.11
;;;;;;;;;;;;;;;;;

;(define (make-account balance)
;  (define (withdraw amount)
;    (if (>= balance amount)
;        (begin (set! balance (- balance amount))
;               balance)
;        "Insufficient funds"))
;  (define (deposit amount)
;    (set! balance (+ balance amount))
;    balance)
;  (define (dispatch m)
;    (cond ((eq? m 'withdraw) withdraw)
;          ((eq? m 'deposit) deposit)
;          (else (error "Unknown request -- MAKE-ACCOUNT"
;                       m))))
;  dispatch)

; =============================================================================

; (define acc (make-account 50))

; +---- global environment------------------------------------+
; | make-account, acc                                         |
; +-----------------------------------------------------------+
;                  |                  |
;               +-+|+       +------------------------------------+
; make-account: |*|*|   E1: | withdraw, deposit, dispatch        |
;               +|+-+       | balance = 50                       |
;                |          +------------------------------------+
; parameters: balance	      |               |                |
; code: make-account body     |               |                |
;                             |               |                |
;			   +-+|+           +-+|+            +-+|+
;		 withdraw: |*|*|  deposit: |*|*|  dispatch: |*|*|
;			   +|+-+           +|+-+            +|+-+
;                           |               |                |
;              parmeters: amount     parmeters: amount    parmeters: m
;              code: withdraw body   code: deposit body   code: dispatch body

; =============================================================================

; ((acc 'deposit) 40)
; 90

; +---- global environment------------------------------------+
; | make-account, acc                                         |
; +-----------------------------------------------------------+
;                  |                  |
;               +-+|+       +------------------------------------+
; make-account: |*|*|   E1: | withdraw, deposit, dispatch        |
;               +|+-+       | balance = 50 + 40                  |------------+
;                |          +------------------------------------+            |
; parameters: balance	      |               |                |              |
; code: make-account body     |               |                |              |
;                             |               |                |              |
;			   +-+|+           +-+|+            +-+|+             |
;		 withdraw: |*|*|  deposit: |*|*|  dispatch: |*|*|             |
;			   +|+-+           +|+-+            +|+-+             |
;                           |               |                |                |
;              parmeters: amount     parmeters: amount    parmeters: m        |
;              code: withdraw body   code: deposit body   code: dispatch body |
;                                                                             |
;                                                                             |
;          +------------------------+-----------------------------------------+
;          |                        |
;     +--------------+         +-------------+
; E2: | m = 'deposit |     E3: | amount = 40 | (deposit 40)
;     +--------------+         +-------------+

; =============================================================================

; ((acc 'withdraw) 60)
; 30

; +---- global environment------------------------------------+
; | make-account, acc                                         |
; +-----------------------------------------------------------+
;                  |                  |
;               +-+|+       +------------------------------------+
; make-account: |*|*|   E1: | withdraw, deposit, dispatch        |
;               +|+-+       | balance = 90 - 60                  |------------+
;                |          +------------------------------------+            |
; parameters: balance	      |               |                |              |
; code: make-account body     |               |                |              |
;                             |               |                |              |
;			   +-+|+           +-+|+            +-+|+             |
;		 withdraw: |*|*|  deposit: |*|*|  dispatch: |*|*|             |
;			   +|+-+           +|+-+            +|+-+             |
;                           |               |                |                |
;              parmeters: amount     parmeters: amount    parmeters: m        |
;              code: withdraw body   code: deposit body   code: dispatch body |
;                                                                             |
;                                                                             |
;          +------------------------+-----------------------------------------+
;          |                        |
;     +---------------+         +-------------+
; E4: | m = 'withdraw |     E5: | amount = 60 | (withdraw 40)
;     +---------------+         +-------------+ 

; =============================================================================

; (define acc2 (make-account 100))

; +---- global environment------------------------------------+
; | make-account, acc, acc2                                   |---------------+
; +-----------------------------------------------------------+               |
;                  |                  |                                       |
;               +-+|+       +------------------------------------+            |
; make-account: |*|*|   E1: | withdraw, deposit, dispatch        |            |
;               +|+-+       | balance = 30                       |            |
;                |          +------------------------------------+            |
; parameters: balance	      |               |                |              |
; code: make-account body     |               |                |              |
;                             |               |                |              |
;			   +-+|+           +-+|+            +-+|+             |
;		 withdraw: |*|*|  deposit: |*|*|  dispatch: |*|*|             |
;			   +|+-+           +|+-+            +|+-+             |
;                           |               |                |                |
;              parmeters: amount     parmeters: amount    parmeters: m        |
;              code: withdraw body   code: deposit body   code: dispatch body |
;                           |               |                |                |
;			   +|+-+           +|+-+            +||-+             |
;		 withdraw: |*|*|  deposit: |*|*|  dispatch: |*|*|             |
;			   +-+|+           +-+|+            +-+|+             |
;                             |               |                |              |
;                             |               |                |              |
;                             |               |                |              |
;                           +------------------------------------+            |
;                       E6: | withdraw, deposit, dispatch        |------------+
;                           | balance = 100                      |             
;                           +------------------------------------+

;;;;;;;;;;;;;;;;;
;;; Exercise 3.12
;;;;;;;;;;;;;;;;;

; (define x (list 'a 'b))
; (define y (list 'c 'd))
; (define z (append x y))
; z
; (a b c d)
; (cdr x)
; <response> = (b)
; (define w (append! x y))
; w
; (a b c d)
; (cdr x)
; <response> = (b c d)

;                +-+-+  +-+-+      +-+-+  +-+-+
;             x--|*|*---|*|/|   y--|*|*---|*|/|
;                +|+-+  +|+-+      +|+-+  +|+-+
;                 |      |          |      |
;                +|+    +|+        +|+    +|+
;                |a|    |b|        |c|    |d|
;                +-+    +-+        +-+    +-+


;                +-+-+  +-+-+      +-+-+  +-+-+
;  (append x y)--|*|*---|*|*-------|*|*---|*|/|
;                +|+-+  +|+-+      +|+-+  +|+-+
;                 |      |          |      |
;                +|+    +|+        +|+    +|+
;                |a|    |b|        |c|    |d|
;                +-+    +-+        +-+    +-+

;                 x                 y
;                 |                 |
;                +-+-+  +-+-+      +-+-+  +-+-+
; (append! x y)--|*|*---|*|*-------|*|*---|*|/|
;                +|+-+  +|+-+      +|+-+  +|+-+
;                 |      |          |      |
;                +|+    +|+        +|+    +|+
;                |a|    |b|        |c|    |d|
;                +-+    +-+        +-+    +-+

;;;;;;;;;;;;;;;;;
;;; Exercise 3.13
;;;;;;;;;;;;;;;;;

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define ex-3.13 (make-cycle '(a b c)))

; Draw a box-and-pointer diagram that shows the structure z created by
; (define z (make-cycle (list 'a 'b 'c)))

;                 +-------------------+
;                 |                   |
;                +-+-+  +-+-+  +-+-+  |
;             z--|*|*---|*|*---|*|*---+
;                +|+-+  +|+-+  +|+-+
;                 |      |      |
;                +|+    +|+    +|+
;                |a|    |b|    |c|
;                +-+    +-+    +-+

; What happens if we try to compute (last-pair z)? 
; - infinite loop happens

;;;;;;;;;;;;;;;;;
;;; Exercise 3.14
;;;;;;;;;;;;;;;;;

;(define (mystery x)
;  (define (loop x y)
;    (if (null? x)
;	 y
;	 (let ((temp (cdr x)))
;	   (set-cdr! x y)
;	   (loop temp x))))
;  (loop x '()))

; x		y
; (1 2 3 4)	()
; (2 3 4)	(1)
; (3 4)		(2 1)
; (4)		(3 2 1)
; ()		(4 3 2 1)

; mystery is destructive reverse!
; it rearanges existing cons cells

;(define v (list 'a 'b 'c 'd))
;(define w (mystery v))

;                      +----+  +----+  +----+  
;                      |    |  |    |  |    |
;                +-+-+ | +-+|+ | +-+|+ | +-+|+
;          v --- |*|/|-+ |*|*|-+ |*|*|-+ |*|*| --- w
;                +|+-+   +|+-+   +|+-+   +|+-+
;                 |       |       |       |
;                +|+     +|+     +|+     +|+
;                |a|     |b|     |c|     |d|
;                +-+     +-+     +-+     +-+

; v = (a)
; w = (d c b a)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.15
;;;;;;;;;;;;;;;;;

;                +---+---+
;          z1 -->| * | * |
;                +-|-+-|-+
;                  V   V
;                +---+---+     +---+---+
;           x -->| * | *-+---->| * | / |
;                +-|-+---+     +-|-+---+
;                  V             V
;                +---+         +---+
;                |wow|         | b |
;                +---+         +---+


;               +---+---+     +---+---+     +---+---+
;         z2 -->| * | *-+---->| * | *-+---->| * | / |
;               +-|-+---+     +-|-+---+     +-|-+---+
;                 |             V             V
;                 |           +---+         +---+
;                 |           | a |         | b |
;                 |           +---+         +---+
;                 |                           ^
;                 |                           |
;                 |           +---+---+     +-|-+---+
;                 +---------->| * | *-+---->| * | / |
;                             +-|-+---+     +---+---+
;                               V
;                             +---+
;                             |wow|
;                             +---+

;;;;;;;;;;;;;;;;;
;;; Exercise 3.16
;;;;;;;;;;;;;;;;;

;       *** 3 ***

;       +---+---+
;  x -->| * | / |
;       +-|-+---+
;         V   
;       +---+---+   +---+---+
;       | / | *---->| / | / |
;       +---+---+   +---+---+

;       *** 4 ***

;       +---+---+
;  x -->| * | *-------+
;       +-|-+---+     |
;         V           V
;       +---+---+   +---+---+
;       | / | *---->| / | / |
;       +---+---+   +---+---+

;       *** 7 ***

;       +---+---+
;  x -->| * | * |
;       +-|-+-|-+
;         V   V   
;       +---+---+
;       | * | * |
;       +-|-+-|-+
;         V   V   
;       +---+---+
;       | / | / |
;       +---+---+


;     *** never ***

;       +---+---+
;  x -->| * | / |<-+
;       +-|-+---+  |
;         V        |
;       +---+---+  |
;       | / | *----+
;       +---+---+

(define (dumb-count-pairs x)
  (if (not (pair? x))
      0
      (+ (dumb-count-pairs (car x))
         (dumb-count-pairs (cdr x))
         1)))

(define one (cons nil nil))
(define two (cons one one))
(define three-3 (cons nil (cons nil one)))
(define three-4 (cons nil two))
(define three-7 (cons two two))
(define neverending (cons nil nil))
(set-cdr! neverending neverending)

(define (test-3.16) 
  (assert '(dumb-count-pairs one) 1)
  (assert '(dumb-count-pairs two) 3)
  (assert '(dumb-count-pairs three-3) 3)
  (assert '(dumb-count-pairs three-4) 4)
  (assert '(dumb-count-pairs three-7) 7))

(test-3.16)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.17
;;;;;;;;;;;;;;;;;

(define (count-pairs x)
  (let ((track nil))
    (define (count-tracked-pairs x)
      (if (or (not (pair? x)) 
	      (memq x track))
	  0
	  (begin
	    (set! track (cons x track))
	    (+ (count-tracked-pairs (car x))
	       (count-tracked-pairs (cdr x))
	       1))))
    (count-tracked-pairs x)))

(define (test-3.17) 
  (assert '(count-pairs one) 1)
  (assert '(count-pairs two) 2)
  (assert '(count-pairs three-3) 3)
  (assert '(count-pairs three-4) 3)
  (assert '(count-pairs three-7) 3)
  (assert '(count-pairs neverending) 1))

(test-3.17)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.18
;;;;;;;;;;;;;;;;;

(define (has-loops x)
  (define (check-tracked-pairs x track)
    (cond ((not (pair? x)) #f)
	  ((memq x track) #t)
	  (else (or (check-tracked-pairs (car x) (cons x track))
		    (check-tracked-pairs (cdr x) (cons x track))))))
  (check-tracked-pairs x nil))

(define (test-3.18) 
  (assert '(has-loops three-4) #f)
  (assert '(has-loops three-7) #f)
  (assert '(has-loops neverending) #t)
  (assert '(has-loops ex-3.13) #t))

(test-3.18)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.19
;;;;;;;;;;;;;;;;;

(define (has-loops-constant-space x)
  (let ((first x)
	(prev nil))
    (define (swap-link x)
      (let ((old (cdr x)))
	(set-cdr! x prev)
	(set! prev x)
	old))
    (define (rewind x)
      (if (not (null? x))
	  (rewind (swap-link x))
	  false))
    (define (do-rewind return tail)
      (let ((old prev))
	(set! prev tail)
	(rewind old)
	return))
    (define (check x)
      (cond ((not (pair? x)) (do-rewind #f nil))
	    ((eq? x first) (do-rewind #t x))
	    (else (check (swap-link x)))))
    (if (not (null? x))	
	(check (swap-link x))
	false)))

(define (test-3.19)
  (assert '(has-loops-constant-space three-4) #f)
  (assert '(has-loops-constant-space three-7) #f)
  (assert '(has-loops-constant-space neverending) #t)
  (assert '(has-loops-constant-space ex-3.13) #t)
  (assert '(has-loops-constant-space (cons 'head ex-3.13)) #t)
  (assert '(has-loops-constant-space (list 1 2 3 4 5 6 7)) #f)
  (assert '(has-loops-constant-space (cons 1 2)) #f))

(test-3.19)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.20
;;;;;;;;;;;;;;;;;

; (define X (cons 1 2))
; (define Z (cons X X))
; (set-car! (cdr Z) 17)
; (car X)

; +---- global environment------------------------------------+
; | cons, car, cdr, set-car!, set-cdr!, X, Z                  |
; +-----------------------------------------------------------+
;                                      |
;     +-+-+       +----------------+   |
;  X: |*|*--------| x = 1,  y = 2  |   |
;     +|+-+       | set-x!, set-y! |---+ (define X (cons 1 2))
;      |          | dispatch       |   |
;      |      E1: +----------------+   |
;  dispatch               |            |
;    code                ref           |
;      |                               |
;     +|+-+       +----------------+   |
;  Z: |*|*--------| x = X  y = X   |   |
;     +|+-+       | set-x!         |---+ (define Z (cons X X))
;                 | set-y!         |   |
;       +---------| dispatch       |   |
;       |     E2: +----------------+   |
;       |                              |
;       |         +----------------+   |
;       |         | z = Z          |---+ (cdr Z)
;       |     E3: +----------------+   |
;       |                              |
;       |         +----------------+   |
;       +---------| m = 'cdr,      |   | (dispatch 'cdr)
;             E4: +----------------+   |           |
;                                      |           |
;                 +----------------+   |           |
;                 | z = X,         |---+ (set-car! X 17)
;                 | new-value = 17 |   |
;      ref    E5: +----------------+   |
;       |                              |
;       |         +----------------+   |
;       +---------| m = 'set-car!  |   | (dispatch 'set-car!)
;       |     E6: +----------------+   |
;       |                              |
;       |         +----------------+   |
;       +---------| v = 17         |   | (set-x! 17)
;       |     E7: +----------------+   |
;       |                              |
;       |         +----------------+   |
;       |         | z = X          |---+ (car X)
;       |     E8: +----------------+    
;       |                               
;       |         +----------------+    
;       +---------| m = 'car,      |     (dispatch 'car)
;             E9: +----------------+    

;;;;;;;;;;;;;;;;;
;;; Exercise 3.21
;;;;;;;;;;;;;;;;;

; Ben sees 'b symbol twice because interpreter
; prints whole queue while printing front
; and last pair while printing rear

(define (print-queue q)
   ; I am not using front-queue because it raises error when q is empty
  (debug (car queue) "\n"))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.22
;;;;;;;;;;;;;;;;;

(define (make-queue)
  (let ((front-ptr nil)
        (rear-ptr nil))
    (define (front-queue)
      (if (null? front-queue)
	  (error "FRONT called with an empty queue")
	  (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
	(if (null? front-ptr)
	    (set! front-ptr new-pair)
	    (set-cdr! rear-ptr new-pair))
	(set! rear-ptr new-pair)
	dispatch))
    (define (delete-queue!)
      (if (null? front-ptr)
	  (error "DELETE! called with an empty queue")
	  (set! front-ptr (cdr front-ptr)))      
      dispatch)
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (null? front-ptr))
	    ((eq? m 'front-queue) (front-queue))
	    ((eq? m 'insert-queue!) insert-queue!)
	    ((eq? m 'list-of-elements) front-ptr)
	    ((eq? m 'delete-queue!) (delete-queue!))
	    ((eq? m 'print-queue) (debug front-ptr "\n"))
	    (else (error "QUEUE DISPATCH -- unknown message" m))))
    dispatch))

(define (insert-queue! q item)
  ((q 'insert-queue!) item))

(define (delete-queue! q)
  (q 'delete-queue!))

(define (front-queue q)
  (q 'front-queue))

(define (empty-queue? q)
  (q 'empty-queue?))

(define (print-queue q)
  (q 'print-queue))

(define (list-of-elements q)
  (q 'list-of-elements))

(define test-queue-a-b-c nil)

(define (test-3.22)
  (set! test-queue-a-b-c (insert-queue! (make-queue) 'a))
  (set! test-queue-a-b-c (insert-queue! test-queue-a-b-c 'b))
  (set! test-queue-a-b-c (insert-queue! test-queue-a-b-c 'c))
  (assert '(list-of-elements test-queue-a-b-c) '(a b c))
  (assert '(list-of-elements (delete-queue! test-queue-a-b-c)) '(b c))
  (assert '(front-queue test-queue-a-b-c) 'b)
  (assert '(list-of-elements (insert-queue! test-queue-a-b-c 'd)) '(b c d)))

(test-3.22)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.23
;;;;;;;;;;;;;;;;;

(define front-part car)
(define rear-part cdr)
(define set-front-part! set-car!)
(define set-rear-part! set-cdr!)
(define queue-contents car)
(define queue-pointers cdr)

(define (make-deque)
  (cons nil nil))

(define (empty-deque? dq)
  (null? (front-part dq)))

(define (deque-get dq fn)
  (if (empty-deque? dq)
      (error fn "on empty deque")
      (queue-contents (fn dq))))

(define (front-deque dq)
  (deque-get dq front-part))

(define (rear-deque dq)
  (deque-get dq rear-part))

(define (insert-deque! dq item get set1 set2)
  (let ((new-item (cons item (cons nil nil))))
    (define (create-from-empty)
      (set-front-part! dq new-item)
      (set-rear-part! dq new-item))
    (define (add-to-non-empty)
      (set2 (queue-pointers (get dq)) new-item)
      (set1 (queue-pointers new-item) (get dq))
      (set1 dq new-item))
    (if (empty-deque? dq)
	(create-from-empty)
	(add-to-non-empty))
    dq))

(define (front-insert-deque! dq item)
  (insert-deque! dq item front-part set-front-part! set-rear-part!))

(define (rear-insert-deque! dq item)
  (insert-deque! dq item rear-part set-rear-part! set-front-part!))

(define (delete-deque! dq get set1 set2)
  (define (delete-non-empty)
    (set1 dq (get (queue-pointers (get dq))))
    (set2 (queue-pointers (get dq)) nil))
  (define (delete-empty)
    (set1 dq nil)
    (set2 dq nil))
  (cond ((empty-deque? dq) (error "DELETE-DEQUE! on empty deque"))
	((eq? (front-part dq) (rear-part dq)) (delete-empty))
	(else (delete-non-empty)))
  dq)

(define (front-delete-deque! dq)
  (delete-deque! dq front-part set-front-part! set-rear-part!))

(define (rear-delete-deque! dq)
  (delete-deque! dq rear-part set-rear-part! set-front-part!))

(define (print-deque dq)
  (define (print-elt elt)
    (if (null? elt)
	false
	(begin
	  (debug (queue-contents elt) " ")
	  (print-elt (front-part (queue-pointers elt))))))
  (debug "< ")
  (print-elt (front-part dq))
  (debug ">\n"))

(define (get-deque-as-list dq)
  (define (get-elt elt)
    (if (null? elt)
	nil
	(cons (queue-contents elt)
	      (get-elt (front-part (queue-pointers elt))))))
  (get-elt (front-part dq)))

(define dq-3.23 (make-deque))

(define (test-3.23)
  (front-insert-deque! dq-3.23 'a)
  (assert '(get-deque-as-list dq-3.23) '(a))
  (front-insert-deque! dq-3.23 'b)
  (assert '(get-deque-as-list dq-3.23) '(b a))
  (rear-insert-deque! dq-3.23 'c)
  (assert '(get-deque-as-list dq-3.23) '(b a c))
  (front-insert-deque! dq-3.23 'd)
  (assert '(get-deque-as-list dq-3.23) '(d b a c))
  (assert '(front-deque dq-3.23) 'd)
  (assert '(rear-deque dq-3.23) 'c)
  (front-delete-deque! dq-3.23)
  (assert '(get-deque-as-list dq-3.23) '(b a c))
  (rear-delete-deque! dq-3.23)
  (assert '(get-deque-as-list dq-3.23) '(b a))
  (rear-delete-deque! dq-3.23)
  (assert '(get-deque-as-list dq-3.23) '(b))
  (front-delete-deque! dq-3.23)
  (assert '(get-deque-as-list dq-3.23) '()))

(test-3.23)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.24
;;;;;;;;;;;;;;;;;

(define (assoc-pred key records predicate)
  (cond ((null? records) false)
	((predicate key (caar records)) (car records))
	(else (assoc-pred key (cdr records) predicate))))

(define (make-table predicate)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc-pred key-1 (cdr local-table) predicate)))
	(if subtable
	    (let ((record (assoc-pred key-2 (cdr subtable) predicate)))
	      (if record
		  (cdr record)
		  false))
	    false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc-pred key-1 (cdr local-table) predicate)))
	(if subtable
	    (let ((record (assoc-pred key-2 (cdr subtable) predicate)))
	      (if record
		  (set-cdr! record value)
		  (set-cdr! subtable
			    (cons (cons key-2 value)
				  (cdr subtable)))))
	    (set-cdr! local-table
		      (cons (list key-1
				  (cons key-2 value))
			    (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
	    ((eq? m 'insert-proc!) insert!)
	    (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define (close? x y)
  (< (abs (- x y)) 0.01))

(define ex-3.24-table nil)

(define (test-3.24)
  (set! ex-3.24-table (make-table close?))
  (assert '((ex-3.24-table 'lookup-proc) 10.000 20.000) #f)
  (assert '((ex-3.24-table 'insert-proc!) 10.000 20.000 'ten-twenty) 'ok)
  (assert '((ex-3.24-table 'lookup-proc) 10.000 20.000) 'ten-twenty)
  (assert '((ex-3.24-table 'lookup-proc) 10.000 21.000) #f)
  (assert '((ex-3.24-table 'lookup-proc) 10.002 20.005) 'ten-twenty))

(test-3.24)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.25
;;;;;;;;;;;;;;;;;

(define (assoc key records)
  (cond ((null? records) false)
	((equal? key (caar records)) (car records))
	(else (assoc key (cdr records)))))

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
	(cdr record)
	false)))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
	(set-cdr! record value)
	(set-cdr! table
		  (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

(define (lookup-table keys table)
  (cond ((not table) table)
	((null? keys) table)	
	(else (lookup-table (cdr keys) (lookup (car keys) table)))))

(define (insert-new-table key table)
  (let ((new-table (make-table)))
    (insert! key new-table table)
    new-table))

(define (insert-table! keys value table)  
  (cond ((null? keys) (error "INSERT-TABLE! -- no keys"))
	((null? (cdr keys)) (insert! (car keys) value table))
	(else (insert-table! (cdr keys) value
			     (or (lookup (car keys) table)
				 (insert-new-table (car keys) table))))))

(define ex-3.25-table nil)
(define ex-3.25-small-table nil)

(define (test-3.25)
  (set! ex-3.25-table (make-table))
  (set! ex-3.25-small-table (make-table))
  (assert '(insert-table! '(a b c) 5 ex-3.25-table) 'ok)
  (assert '(insert-table! '(a b d) 6 ex-3.25-table) 'ok)
  (assert '(insert-table! '(1 2)   7 ex-3.25-table) 'ok)
  (assert '(insert-table! '("x")   8 ex-3.25-table) 'ok)

  (assert '(lookup-table '(a b c) ex-3.25-table) 5)
  (assert '(lookup-table '(a b d) ex-3.25-table) 6)
  (assert '(lookup-table '(1 2) ex-3.25-table) 7)
  (assert '(lookup-table '("x") ex-3.25-table) 8)

  (assert '(insert-table! '(c) 5 ex-3.25-small-table) 'ok)
  (assert '(insert-table! '(d) 6 ex-3.25-small-table) 'ok)

  (assert '(lookup-table '(a b) ex-3.25-table) ex-3.25-small-table))

(test-3.25)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.26
;;;;;;;;;;;;;;;;;

(define tbl-key caar)
(define tbl-val cdar)

(define l-branch cadr)
(define r-branch cddr)

(define (sym>? a b)
  (string>? 
   (symbol->string a)
   (symbol->string b)))

(define (tbl-greater a b)
  (cond ((and (number? a) (number? b)) (> a b))
	((and (symbol? a) (symbol? b)) (sym>? a b))
	((and (string? a) (string? b)) (string>? a b))
	((number? a) #f) ; any number is lower than any string or symbol
	((symbol? a) #t) ; any symbol is higher than any number or string
	((and (string? a) (number? b)) #t)
	((and (string? a) (symbol? b)) #f)
	(else (error "TBL-GREATER -- error in types"))))

(define (assoc key tree)
  (cond ((null? tree) false)
	((tbl-greater key (tbl-key tree)) (assoc key (r-branch tree)))
	((tbl-greater (tbl-key tree) key) (assoc key (l-branch tree)))
	(else (car tree))))

(define (insert-in-tree! key value tree)
  ; insert-in-tree! is called from insert! 
  ; which assures that key is not in the tree
  (define (set-or-traverse get set)
    (if (not (null? (get tree)))
	(insert-in-tree! key value (get tree))
	(set (cdr tree) (cons (cons key value) (cons nil nil)))))
  (if (tbl-greater key (tbl-key tree))
      (set-or-traverse r-branch set-cdr!)
      (set-or-traverse l-branch set-car!)))

(define (lookup key table)
  (let ((record (assoc key table)))
    (and record (cdr record))))

(define (insert! key value table)
  (let ((record (assoc key table)))
    (if record
	(set-cdr! record value)
	(insert-in-tree! key value table)))
  'ok)

(define (make-table)
  (cons (cons '*table* nil) (cons nil nil)))

(test-3.25)

(define ex-3.26-table nil)

(define (test-3.26)
  (set! ex-3.26-table (make-table))
  (assert '(insert! 8 'a ex-3.26-table) 'ok)
  (assert '(insert! 4 'b ex-3.26-table) 'ok)
  (assert '(insert! 9 'c ex-3.26-table) 'ok)
  (assert '(insert! 2 'd ex-3.26-table) 'ok)
  (assert '(insert! 6 'e ex-3.26-table) 'ok)
  (assert '(lookup  9 ex-3.26-table) 'c)
  (assert '(lookup  2 ex-3.26-table) 'd)
  (assert '(lookup  4 ex-3.26-table) 'b)
  (assert '(lookup  6 ex-3.26-table) 'e)
  (assert '(lookup  8 ex-3.26-table) 'a))

(test-3.26)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.27
;;;;;;;;;;;;;;;;;

(define (fib n)
  (debug "fib = " n "\n")
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
	     (debug "memo-fib = " n "\n")
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

; (define memo-fib (memoize fib))
; is bad because recursive calls to fib will not be memoized

; environment diagram in ex-3.27.png

; memo-fib can be executed in O(n), because for memo-fib(n) 
; body is executed n times, all other calls is satified with table lookup,
; (once calls to memo-fib(0), memo-fib(1) ... memo-fib(n) are done
; corresponding values is cached and looked up in table in all other calls)
; when calling with empty table fib(n) = fib(n-1) + fib(n-2),
; fib(n-1) is evaluated, within fib(n-1) call fib(n-2) is evaluated
; and remembered, therefore within fib(n) value to fib(n-2) is taken
; from table, thus for fib(n) there is about n table lookups and insertions
; If table lookups and insertions can be done in constant time, then
; memoized fib is O(n), else if it takes O(f(n)) to do lookups or insertions
; then memoized fib takes O(n*f(n)) time

; structure of fib is such that fib(0) will be added to table first then
; imediately looked up, then fib(1) will be added and looked up and so on 
; until fib(n) will be added last

; the way unordered list implementation of tables is built inserting in
; table takes constant amount of time, while looking up can take up to
; m steps where m is size of table, but in our implementation of 
; memoized fib looking up element that we just added to table
; will also take constant time

; ofcourse if table is already filled with m entries and we are calculating
; fib(n), where m >> n (much greater), then algorithm is suboptimal

; this could be guaranteed O(n) (just use fresh table to each fib calculation)

;(define (smart-memo-fib n)
;  (define memo-fib
;    (memoize (lambda (n)
;	       (cond ((= n 0) 0)
;		     ((= n 1) 1)
;		     (else (+ (memo-fib (- n 1))
;			      (memo-fib (- n 2))))))))
;  (memo-fib n))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.28
;;;;;;;;;;;;;;;;;

(define (logical-or a b)
  (cond ((and (= a 0) (= b 0)) 0)
	((and (= a 1) (= b 0)) 1)
	((and (= a 0) (= b 1)) 1)
	((and (= a 1) (= b 1)) 1)
        (else (error "Invalid signals" a b))))

(define (or-gate i1 i2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal i1) (get-signal i2))))
      (after-delay or-gate-delay
                   (lambda () 
		     (set-signal! output new-value)))))
  (add-action! i1 or-action-procedure)
  (add-action! i2 or-action-procedure)
  'ok)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.29
;;;;;;;;;;;;;;;;;

(define (or-gate i1 i2 output)
  (let ((n1 (make-wire))
	(n2 (make-wire))
	(no (make-wire)))
    (inverter i1 n1)
    (inverter i2 n2)
    (and-gate n1 n2 no)
    (inverter no output)))

; or-gate-delay = and-gate-delay + 2 * inverter-delay
; + inverter-deley, because first two inverters work in parallel
; + and-gate-delay, for (and-gate n1 n2 no) gate
; + inverter-deley, for final inverter (inverter no output)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.30
;;;;;;;;;;;;;;;;;

(define (ripple-carry-adder a b s c)
  (if (and (pair? a) (pair? b) (pair? s))
      (let ((new-c (make-wire)))
	(full-adder (car a) (car b) new-c (car s) c) 
	(ripple-carry-adder (cdr a) (cdr b) (cdr s) new-c))
      'ok))  

; ripple-carry-adder-delay = n * full-adder-delay
; full-adder-delay = 2 * half-adder-delay + or-delay
; half-adder-delay = max(and-delay + not-delay, or-delay) + and-delay

; assume that or-delay = and-delay + 2 * not-delay (from 3.29)

; then half-adder-delay = 2 * and-delay + 2 * not-delay
; then full-adder-delay = 5 * and-delay + 6 * not-delay

; then ripple-carry-adder-delay = 5n * and-delay + 6n * not-delay

;;;;;;;;;;;;;;;;;
;;; Exercise 3.31
;;;;;;;;;;;;;;;;;

; if new action procedure is not imediately run,
; then initial values of wires is not propagated

; for example:
; (let ((a (make-wire))
;       (b (make-wire)))
;    (inverter a b))

; both wires a and b is initialized to 0, after that we add inverter,
; if accept-action-procedure! in inverter does not call action procedure, then
; (after-delay inverter-delay (lambda () (set-signal! output new-value)))))
; is never called and after (propagate), output b that should be 1 is still 0

; if we have defined accept-action-procedure! as:

; (define (accept-action-procedure! proc)
;   (set! action-procedures (cons proc action-procedures)))

;              +--------------------------------------+
;              |         ____                         |
;          A --------*---\   \ D               ___    |
;              |     |    >1  >---------------|   \   |
;              |  +--|---/___/                | 4  )----- S
;              |  |  |              |\  E  +--|___/   |
;              |  |  |           +--|3>o---+          |
;              |  |  |    ___    |  |/                |
;              |  |  +---|   \   |                    |
;              |  |      | 2  )--*----------------------- C
;          B -----*------|___/                        |
;              |                                      |
;              +--------------------------------------+

; then half-adder would make wrong output s = 0, c = 0 for input a = 0, b = 1

; it will happen because not-gate(3) will not flip wire C signal into wire E
; and-gate(2) will output 0 (because a = 0, b = 1), 
; then in not-gate(3) set-my-signal! will check
; that (= signal-value new-value) is true and will not
; (call-each action-procedures) and thus wire E will not be fliped to 1
; and that would yield wrong output on S

;;;;;;;;;;;;;;;;;
;;; Exercise 3.32
;;;;;;;;;;;;;;;;;

; proper QUEUE implementation
; ===========================

;          ___
;    0 -a-|   \
;         |and )-c- 0
;    1 -b-|___/

; apply 1 to a

;          ___
;    1 -a-|   \
;         |and )-c- 0
;    1 -b-|___/

; put in queue that after time t - (X) (a=1, b=1) yields c=1, apply 0 to b

;          ___
;    1 -a-|   \
;         |and )-c- 0
;    0 -b-|___/

; put in queue that after time t - (Y) (a=1, b=0) yields c=0

;          ___
;    1 -a-|   \
;         |and )-c- 1
;    0 -b-|___/

; after some time take out of queue (X) and set c=1

;          ___
;    1 -a-|   \
;         |and )-c- 0
;    0 -b-|___/

; take out of queue (Y) and set c=0

; wrong LIST/STACK implementation
; ===============================

;          ___
;    0 -a-|   \
;         |and )-c- 0
;    1 -b-|___/

; apply 1 to a

;          ___
;    1 -a-|   \
;         |and )-c- 0
;    1 -b-|___/

; put in stack that after time t - (X) (a=1, b=1) yields c=1, apply 0 to b

;          ___
;    1 -a-|   \
;         |and )-c- 0
;    0 -b-|___/

; put in stack that after time t - (Y) (a=1, b=0) yields c=0

;          ___
;    1 -a-|   \
;         |and )-c- 0
;    0 -b-|___/

; after some time take out of stack (Y) and set c=0

;          ___
;    1 -a-|   \
;         |and )-c- 1
;    0 -b-|___/

; take out of stack (X) and set c=1

(load "simulator")

;;;;;;;;;;;;;;;;;
;;; Exercise 3.33
;;;;;;;;;;;;;;;;;

(load "constraint")

(define (averager a b c)
  (let ((two (make-connector))
	(sum (make-connector)))
    (constant 2 two)
    (multiplier c two sum)
    (adder a b sum)
    'ok))

(define (test-constraint fn . values)
  (define (fill-connector val)
    (let ((connector (make-connector)))
      (if val (set-value! connector val 'user) false)
      connector))
  (let ((connectors (map fill-connector values)))
    (apply fn connectors)
    (map get-value connectors)))

(assert '(test-constraint averager 1.0 2.0 false) '(1.0 2.0 1.5))
(assert '(test-constraint averager 3.0 false 2.5) '(3.0 2.0 2.5))
(assert '(test-constraint averager false 6.0 5.0) '(4.0 6.0 5.0))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.34
;;;;;;;;;;;;;;;;;

;(define (squarer a b)
;  (multiplier a a b))

;> (test-constraint squarer 5 false)
;(5 25)
;> (test-constraint squarer false 5)
;(#f 5)

; the problem is that multiplier can figure out third value from two known ones
; in (multiplier a a b) if b is supplied there is still two unknown values

;;;;;;;;;;;;;;;;;
;;; Exercise 3.35
;;;;;;;;;;;;;;;;;

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
	(if (< (get-value b) 0)
	    (error "square less than 0 -- SQUARER" (get-value b))
	    (set-value! a (sqrt (get-value b)) me))
	(if (has-value? a)
	    (set-value! b (square (get-value a)) me)
	    'ignored)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(assert '(test-constraint squarer 5 false) '(5 25))
(assert '(test-constraint squarer false 36) '(6 36))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.36
;;;;;;;;;;;;;;;;;

; +---- global environment------------------------------------+
; | A, B, make-connector, set-value!, for-each-except         |
; +-----------------------------------------------------------+
;                                             |
;     +-+-+       +------------------------+  |
;  B: |*|*--------| value = false          |  |
;     +|+-+       | informant = false      |--+ (define a (make-connector))
;      |      E2: | constraints = '()      |  |
;      |          +------------------------+  |
; make-connector                              |
;    code                                     |
;      |                                      |
;     +|+-+       +------------------------+  |
;  A: |*|*--------| value = 10             |  |
;     +|+-+       | informant = 'user      |--+ (define b (make-connector))
;             E1: | constraints = '()      |  |
;                 +------------------------+  |
;                             |               |
;      +----------------------+               |
;      |                                      |
;      |          +------------------------+  |
;      |          | connector = A          |  |
;      |          | new-value = 10         |--+ (set-value! a 10 'user)
;      |      E3: | informant = 'user      |  |
;      |          +------------------------+  |
;      |                                      |
;      |          +------------------------+  |
;      +----------| request = 'set-value!  |  |
;      |          |                        |  | (connector 'set-value!)
;      |      E4: |                        |  |
;      |          +------------------------+  |
;      |                                      |
;      |          +------------------------+  |
;      +----------| newval = 10            |  |
;                 | setter = 'user         |  | (set-my-value 10 'user)
;             E5: |                        |  |
;                 +------------------------+  |
;                                             |
;         +--------------------------------+  |
;         | exception = 'user              |  |
;         | procedure = inform-about-value |--+ (for-each-except 
;     E6: | list = '()                     |      'user inform-about-value '())
;         +--------------------------------+

;;;;;;;;;;;;;;;;;
;;; Exercise 3.37
;;;;;;;;;;;;;;;;;

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- z y)
  (let ((x (make-connector)))
    (adder x y z)
    x))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ z y)
  (let ((x (make-connector)))
    (multiplier x y z)
    x))

(define (cv n)
  (let ((c (make-connector)))
    (constant n c)
    c))

(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
	  x)
      (cv 32)))

(define celsius (make-connector))
(define fahrenheit (celsius-fahrenheit-converter celsius))

(define (test-3.37)  
  (forget-value! celsius 'user)
  (forget-value! fahrenheit 'user)
  (set-value! celsius 25 'user)
  (assert '(get-value fahrenheit) 77)
  (forget-value! celsius 'user)
  (set-value! fahrenheit 212 'user)
  (assert '(get-value celsius) 100))

(test-3.37)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.38
;;;;;;;;;;;;;;;;;

; A.

; peter, paul, mary ((100 + 10) - 20) / 2 = 45
; peter, mary, paul ((100 + 10) / 2) - 20 = 35
; paul, mary, peter ((100 - 20) / 2) + 10 = 50
; paul, peter, mary ((100 - 20) + 10) / 2 = 45
; mary, paul, peter ((100 / 2) - 20) + 10 = 40
; mary, peter, paul ((100 / 2) + 10) - 20 = 40

; B.
;
; peter			paul			mary

; --------------------------------------------------
; read 100		read 100		read 100
; modify 110		modify 80		modify 50
; pause			pause			write 50
; pause			write 80		pause
; write 110		pause			pause
; ===================================================
; final balance 110

; ---------------------------------------------------
; read 100		read 100		read 100
; modify 110		modify 80		modify 50
; pause			pause			write 50
; write 110		pause			pause
; pause			write 80		pause
; ===================================================
; final balance 80

; ---------------------------------------------------
; read 100		pause			read 100
; modify 110		pause			modify 50
; pause			pause			write 50
; pause			read 50			pause
; write 110		modify 30		pause
; pause			write 30		pause
; ===================================================
; final balance 30

; ---------------------------------------------------
; read 100		pause			pause
; modify 110		read 100		pause
; write 110		pause			pause
; pause			modify 80		read 110
; pause			write 80		modify 55
; pause			pause			write 55
; ===================================================
; final balance 55

; ---------------------------------------------------
; read 100		pause			read 100
; modify 110		pause			modify 50
; pause			pause			write 50
; write 110		pause			pause
; pause			read 110		pause
; pause			modify 90		pause
; pause			write 90		pause
; ===================================================
; final balance 90

; ---------------------------------------------------
; pause			read 100		read 100
; pause			modify 80		modify 50
; pause			write 80		pause
; pause			pause			write 50
; read 50		pause			pause
; modify 60		pause			pause
; write 60		pause			pause
; ===================================================
; final balance 60

;;;;;;;;;;;;;;;;;
;;; Exercise 3.39
;;;;;;;;;;;;;;;;;

;(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
;                  (s (lambda () (set! x (+ x 1)))))

; (* x x)		== squarex
; (set! x squarex)	== setx
; (set! x (+ x 1))	== incx

; three possibilities remain:

; incx, squarex, setx	== 121
; squarex, incx, setx	== 11
; squarex, setx, incx	== 101

;;;;;;;;;;;;;;;;;
;;; Exercise 3.40
;;;;;;;;;;;;;;;;;

; unprotected:
; get-2-x get-2-x set-2-x get-3-x get-3-x get-3-x set-3-x = 1000000
; get-2-x get-2-x get-3-x set-2-x get-3-x get-3-x set-3-x = 100000
; get-2-x get-2-x get-3-x get-3-x set-2-x get-3-x set-3-x = 10000
; get-2-x get-2-x get-3-x get-3-x get-3-x set-2-x set-3-x = 1000
; get-2-x get-2-x get-3-x get-3-x get-3-x set-3-x set-2-x = 100
; get-2-x get-3-x get-3-x get-3-x set-3-x get-2-x set-2-x = 10000
; get-3-x get-3-x get-3-x set-3-x get-2-x get-2-x set-2-x = 1000000

; serialized:
; (get-2-x get-2-x set-2-x) (get-3-x get-3-x get-3-x set-3-x) = 1000000
; (get-3-x get-3-x get-3-x set-3-x) (get-2-x get-2-x set-2-x) = 1000000

;;;;;;;;;;;;;;;;;
;;; Exercise 3.41
;;;;;;;;;;;;;;;;;

; In current example Ben's change is not necessary,
; because balance is consistent at all times

; on other hand, Ben's change would be necessary,
; if there were procedure that put in balance some temporary value

;;;;;;;;;;;;;;;;;
;;; Exercise 3.42
;;;;;;;;;;;;;;;;;

; I think that it is safe to make such change.
; And I think that it will be more efficient.

;;;;;;;;;;;;;;;;;
;;; Exercise 3.43
;;;;;;;;;;;;;;;;;

; ex-3.43.png

;;;;;;;;;;;;;;;;;
;;; Exercise 3.44
;;;;;;;;;;;;;;;;;

; Louis is not right.
; This kind of transfer is good.
; It differs from exchange in that manipulations 
; on from-account and to-account is "atomic" (because serialized)
; exchange first extrancts balances to calculate difference
; and then later changes balances depending of difference calculated
; bad things may happen between difference calculation and balance changing,
; there is no such thing tranfer procedure

;;;;;;;;;;;;;;;;;
;;; Exercise 3.45
;;;;;;;;;;;;;;;;;

; serialized-exchange will serialize exchange with account's serializer
; and run it - so any other procedure serialized with that serializer
; would not be able to run until exchane will be finished,
; but within exchange evaluation of ((account 'deposit) amount) 
; will result in a call to make-account's internal deposit procedure
; which *is* serialized with the same account's serializer.

; thus deadlock will ensue me thinks

;;;;;;;;;;;;;;;;;
;;; Exercise 3.46
;;;;;;;;;;;;;;;;;

; ex-3.46.png

;;;;;;;;;;;;;;;;;
;;; Exercise 3.47
;;;;;;;;;;;;;;;;;

(load "parallel")

(define mutser (make-serializer))

(define test-and-set!
  (mutser
   (lambda (cell)
     (if (car cell)
	 true
	 (begin (set-car! cell true)
		false)))))

(define clear!
  (mutser (lambda (cell) (set-car! cell false))))

(define (make-mutex)
  (let ((cell (list false)))            
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

; a.
(define (make-semaphore n)
  (let ((lock (make-mutex)))
    (define (acquire)
      (let ((acquired? false))
	(lock 'acquire)
	(if (> n 0)
	    (begin
	      (set! acquired? true)
	      (set! n (- n 1))))	
	(lock 'release)
	(if (not acquired?)
	    (acquire))))
    (define (release)
      (lock 'acquire)
      (set! n (+ n 1))
      (lock 'release))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
	    ((eq? m 'release) (release))
	    (else (error "SEMAPHORE -- unknown message" m))))
    the-semaphore))

; b.
(define atomic-inc!
  (mutser
   (lambda (cell)
     (set-car! cell (+ (car cell) 1)))))

(define test-and-dec!
  (mutser
   (lambda (cell)
     (if (= 0 (car cell))
	 false
	 (begin
	   (set-car! cell (- (car cell) 1))
	   true)))))

(define (make-semaphore n)
  (let ((cell (list n)))
    (define (acquire)
      (if (not (test-and-dec! cell)) (acquire)))
    (define (release)
      (atomic-inc! cell))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire) (acquire))
	    ((eq? m 'release) (release))
	    (else (error "SEMAPHORE -- unknown message" m))))
    the-semaphore))

(define (test-3.47)
  (let ((sem (make-semaphore 3)))
    (parallel-execute
     (lambda ()        
       (debug "\n")
       (sem 'acquire)
       (debug "acquired 1\n")
       (sem 'acquire)
       (debug "acquired 2\n")
       (sem 'acquire)
       (debug "acquired 3\n")
       (sem 'acquire)
       (debug "acquired 4\n")
       (sem 'acquire)
       (debug "acquired 5\n"))
     (lambda ()
       (sleep 1)
       (debug "releasing...\n")
       (sem 'release)
       (sleep 1)
       (debug "releasing...\n")
       (sem 'release)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.48
;;;;;;;;;;;;;;;;;

; deadlock avoidance by assigning each account a unique number and then
; acquiring locks in order of that number solves deadlocking in exchange.

; if exchange wants swap balances of accounts a and b, such that a.id < b.id
; and aquires lock to a, but lock to b is already taken, then there is two
; cases, some process holds b as "second" lock and it will be freed soon
; or some process holds lock to b as "first" lock and is waiting for "second"
; lock of some account c, such that b.id < c.id (by algorithm definition)
; this makes situation of accounts b and c the same as with accounts a and b
; applying this reasoning somewhat inductively we can come to conclusion,
; that because we have account with maximum id, then lock to that account can 
; be aquired only as "second" lock and that eventualy it will make also it's
; "first" lock avaible at some time, thus making it avaible for some
; process to use it as "second" lock

(define account-id
  (let ((num 0))
    (mutser
     (lambda ()
       (set! num (inc num))
       num))))

(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer))
	(id (account-id)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'get-id) id)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT"
                         m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (procrastinate time fn)
  (lambda args (sleep time) (apply fn args)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (procrastinate 1 (serializer2 exchange)))
     account1
     account2)))

(define (serialized-exchange-no-deadlock account1 account2)
  (let ((id1 (account1 'get-id))
	(id2 (account2 'get-id)))
    (if (< id1 id2)
	(serialized-exchange account1 account2)
	(serialized-exchange account2 account1))))

(define (test-3.48 fn)
  (let ((a1 (make-account-and-serializer 100))
	(a2 (make-account-and-serializer 150)))
    (parallel-execute
     (lambda ()
       (fn a1 a2)
       (debug "done - serialized-exchange a1 a2\n"))
     (lambda ()
       (serialized-exchange a2 a1)
       (debug "done - serialized-exchange a2 a1\n")))))

(define (deadlock)
  (test-3.48 serialized-exchange))

(define (no-deadlock)
  (test-3.48 serialized-exchange-no-deadlock))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.49
;;;;;;;;;;;;;;;;;

; if there were a bank account such that every time something is deposited
; to it, it deposits %1 of amount further to some other "charity" account

; how suppose that we serial-exchange such bank account 
; with it's corresponding "charity" account

; serial-exchange grabs locks to both accounts, but
; while depositing %1 to "charity",
; deposit procedure tries to grab
; lock to "charity" account and fails...
; ...deadlock

;;;;;;;;;;;;;;;;;
;;; Exercise 3.50
;;;;;;;;;;;;;;;;;

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream 
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (local-stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply local-stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (listify-stream s)
  (if (stream-null? s)
      nil
      (cons (stream-car s) (listify-stream (stream-cdr s)))))

(assert '(listify-stream
	  (local-stream-map 
	   +
	   (stream-enumerate-interval 10 20)
	   (stream-enumerate-interval 20 30)))
	'(30 32 34 36 38 40 42 44 46 48 50))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.51
;;;;;;;;;;;;;;;;;

(define (show x)
  (display-line x)
  x)

; 1 ]=> (define x (stream-map show (stream-enumerate-interval 0 10)))
;
; 0
; ;Value: x
;
; 1 ]=> (stream-ref x 5)
;
; 1
; 2
; 3
; 4
; 5
; ;Value: 5
;
; 1 ]=> (stream-ref x 7)
;
; 6
; 7
; ;Value: 7

;;;;;;;;;;;;;;;;;
;;; Exercise 3.52
;;;;;;;;;;;;;;;;;

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; What is the value of sum after each of
;;; the above expressions is evaluated?

; (define sum 0)
; sum == 0

; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; sum == 0

; (define seq (stream-map accum (stream-enumerate-interval 1 20)))
; sum == 1

; (define y (stream-filter even? seq))
; sum == 6

; (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))
; sum == 10

; (stream-ref y 7)
; sum == 136

; (display-stream z)
; sum == 210

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; What is the printed response to evaluating 
;;; the stream-ref and display-stream  expressions?

; (stream-ref y 7)

; ;Value: 136

; (display-stream z)

; 10
; 15
; 45
; 55
; 105
; 120
; 190
; 210
; ;Unspecified return value

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Would these responses differ if we had implemented
;;; (delay <exp>) simply as (lambda () <exp>) without
;;; using the optimization provided by memo-proc ? Explain.

; ofcourse there is difference

; delay/force

; (define sum 0)
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; (define d (delay (accum 10)))

; (force d)
; sum == 10
; (force d)
; sum == 10
; (force d)
; sum == 10

; lambda/funcall

; (define sum 0)
; (define (accum x)
;   (set! sum (+ x sum))
;   sum)
; (define d (lambda () (accum 10)))

; (d)
; sum == 10
; (d)
; sum == 20
; (d)
; sum == 30

; the difference is just that
; delay/force does not repeat calculation
; but lambda/funcall does repeat calculation
; if there is assignment in calculation
; then hilarity ensues

;;;;;;;;;;;;;;;;;
;;; Exercise 3.53
;;;;;;;;;;;;;;;;;

;	    1
;  1 + 1  = 2
;  2 + 2  = 4
;  4 + 4  = 8
;  8 + 8  = 16
; 16 + 16 = 32
; 32 + 32 = 64
; ...

;;;;;;;;;;;;;;;;;
;;; Exercise 3.54
;;;;;;;;;;;;;;;;;

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1 (add-streams ones integers)))

(define factorials (cons-stream 1 (mul-streams integers factorials)))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.55
;;;;;;;;;;;;;;;;;

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (stream-cdr s) (partial-sums s))))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.56
;;;;;;;;;;;;;;;;;

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car) 
		  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (is-2-3-5? x)
  (if (= x 1)
      true
      (cond ((= (remainder x 2) 0) (is-2-3-5? (/ x 2)))
	    ((= (remainder x 3) 0) (is-2-3-5? (/ x 3)))
	    ((= (remainder x 5) 0) (is-2-3-5? (/ x 5)))
	    (else false))))

(define S (cons-stream 1 (merge (scale-stream S 2)
				(merge (scale-stream S 3)
				       (scale-stream S 5)))))

(define ex-3.56 S)

(define (test-3.56 n)
  (assert (lambda () (is-2-3-5? (stream-ref ex-3.56 n))) true)
  (if (> n 0) (test-3.56 (- n 1))))

(test-3.56 100)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.57
;;;;;;;;;;;;;;;;;

; delay/force needs (n - 2) additions because once nth number have been
; computed it's value is remembered by memo-proc and returned next time
; delay is forced, so basically it takes one addition per fibonacci number

; lambda/funcall
; if we implement delay/force with lambda and function call then there is
; no remembering, to calculate fib(n) we must calculate:

; fibadds(0) is hardcoded (0 additions)
; fibadds(1) is hardcoded (0 additions)
; fibadds(2) = 1 + fibadds(0) + fibadds(1) = 1
; fibadds(3) = 1 + fibadds(1) + fibadds(2) = 2
; fibadds(4) = 1 + fibadds(2) + fibadds(3) = 4
; fibadds(5) = 1 + fibadds(3) + fibadds(4) = 7
; fibadds(6) = 1 + fibadds(4) + fibadds(5) = 12
; fibadds(7) = 1 + fibadds(5) + fibadds(6) = 20
; fibadds(8) = 1 + fibadds(6) + fibadds(7) = 36
; fibadds(9) = 1 + fibadds(7) + fibadds(8) = 57

; fibadds(n) >= fib(n) for all n > 2
; because fib(n) = fib(n - 1) + fib(n - 2)
; while fibadds(n) = 1 + fibadds(n - 1) + fibadds(n - 2)

; as we have seen in 1.2.2 fib(n) is approximately phi^n/sqrt(5)
; and that makes amount of additions at least exponential

;;;;;;;;;;;;;;;;;
;;; Exercise 3.58
;;;;;;;;;;;;;;;;;

(define (display-stream-n s n)
  (display-line (stream-car s))
  (if (> n 1)
      (display-stream-n (stream-cdr s) (- n 1))))

(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

; (expand 1 7 10)

; repeating (1 4 2 8 5 7)

; 1/7 = 0.142857142857142857...

; (expand 3 8 10)

; (3 7 5) repeating (0)

; 3/8 = 0.37500000...

; expand generates fraction of numerator/denumerator in radix base
; denumerator > numerator, i guess

;;;;;;;;;;;;;;;;;
;;; Exercise 3.59
;;;;;;;;;;;;;;;;;

(define (listify-stream-n s n)
  (if (or (stream-null? s) (= n 0))
      nil
      (cons (stream-car s) (listify-stream-n (stream-cdr s) (- n 1)))))

; A.

(define (div-streams s1 s2)
  (stream-map / s1 s2))

(define (integrate-series s)
  (mul-streams s (div-streams ones integers)))

; B.

(define minus-ones (cons-stream -1 minus-ones))

(define cosine-series
  (cons-stream 1 (integrate-series (mul-streams minus-ones sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(assert '(listify-stream-n sine-series 6)
	'(0 1 0 -1/6 0 1/120))

(assert '(listify-stream-n cosine-series 6)
	'(1 0 -1/2 0 1/24 0))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.60
;;;;;;;;;;;;;;;;;

(define (constant-stream x)
  (cons-stream x (constant-stream x)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
	       (add-streams (mul-series s1 (stream-cdr s2))
			    (mul-streams (stream-cdr s1)
					 (constant-stream
					  (stream-car s2))))))

(assert '(accumulate + 0.0
		     (listify-stream-n
		      (add-streams
		       (mul-series sine-series sine-series)
		       (mul-series cosine-series cosine-series))
		      100))
	1.0)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.61
;;;;;;;;;;;;;;;;;

(define (negate-stream s)
  (stream-map - s))

(define (invert-unit-series S)
  (cons-stream 1 (mul-series (negate-stream (stream-cdr S))
			     (invert-unit-series S))))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.62
;;;;;;;;;;;;;;;;;

(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
      (error "DIV-SERIES divide by zero")
      (mul-series s1 (invert-unit-series s2))))

(assert '(listify-stream-n (div-series sine-series cosine-series) 6)
	'(0 1 0 1/3 0 2/15))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.63
;;;;;;;;;;;;;;;;;

; by using guesses we reuse already forced cells of the guesses stream 

; if on other hand we use (sqrt-stream x) all the computation is redone,
; because it makes different stream object with the same contents

; when using (lambda () <exp>) there is no difference between approaches,
; because efficiency gained in first case was due to memo-proc in delay

;;;;;;;;;;;;;;;;;
;;; Exercise 3.64
;;;;;;;;;;;;;;;;;

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0 (stream-map (lambda (g) (sqrt-improve g x)) guesses)))
  guesses)

(define (stream-limit s tolerance)
  (let ((x0 (stream-car s))
	(x1 (stream-car (stream-cdr s))))
    (if (< tolerance (abs (- x1 x0)))
	(stream-limit (stream-cdr s) tolerance)
	x1)))	

(define (stream-sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(assert '(stream-sqrt 2 0.01)
	(lambda (x) (< (- (sqrt 2) 0.01) x (+ (sqrt 2) 0.01))))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.65
;;;;;;;;;;;;;;;;;

(define (ln2-summands n)
  (cons-stream (/ 1 n) (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1.0)))

; I read that this is called aitken acceleration
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))           ; Sn-1
        (s1 (stream-ref s 1))           ; Sn
        (s2 (stream-ref s 2)))          ; Sn+1
    (let ((denom (+ s0 (* -2 s1) s2)))      
      (cons-stream (- s2 (if (= denom 0.0) 0.0 (/ (square (- s2 s1)) denom)))
		   (euler-transform (stream-cdr s))))))

(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(define (test-3.65)
  (display-stream-n ln2-stream 50)
  (newline)
  (display-stream-n (euler-transform ln2-stream) 50)
  (newline)
  (display-stream-n (accelerated-sequence euler-transform ln2-stream) 50))

; 50 iterations yields:

; ln2-stream
; .6832471605759183
; precision 1 place

; accelerated ln2-stream
; .6931462658490956
; precision 5 places

; super accelerated ln2-stream
; .6931471805599454
; precision 15 places

;;;;;;;;;;;;;;;;;
;;; Exercise 3.66
;;;;;;;;;;;;;;;;;

; 0.(1,1)  1.(1,2)  3.(1,3)  5.(1,4)  7.(1,5)  9.(1,6) 11.(1,7)
;
;          2.(2,2)  4.(2,3)  8.(2,4) 12.(2,5) 16.(2,6) 20.(2,7)
;
;                   6.(3,3) 10.(3,4) 18.(3,5) 26.(3,6) 34.(3,7)
;
;                           14.(4,4) 22.(4,5) 38.(4,6) 54.(4,7)
;
;                                    30.(5,5) 46.(5,6) 78.(5,7)

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1) (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave 
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (find-in-stream stream something)
  (define (find s n)
    (if (equal? something (stream-car s))
	n
	(find (stream-cdr s) (+ n 1))))
  (find stream 0))
    
(define (pair-num i j)
  (let ((step (expt 2 i)))
    (+ -2 (if (= i j)
	      step
	      (+ (/ step 2)
		 (* step (- j i)))))))

(define integer-pairs (pairs integers integers))

(define (test-3.66 i j)
  (assert (list 'pair-num i j) (find-in-stream integer-pairs (list i j))))

(test-3.66 11 11)
(test-3.66 7 12)
(test-3.66 5 17)
	
; For example, about how many pairs precede the pair (1,100)?
; 197

; the pair (99,100)?
; 950737950171172051122527404030

; the pair (100,100)? 
; 1267650600228229401496703205374

;;;;;;;;;;;;;;;;;
;;; Exercise 3.67
;;;;;;;;;;;;;;;;;

(define (full-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave 
    (interleave 
     (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
     (stream-map (lambda (x) (list x (stream-car t))) (stream-cdr s)))
    (full-pairs (stream-cdr s) (stream-cdr t)))))
;        ^
;        |
;        |
; There was copy paste bug here. I forgot to replace pairs with full-pairs
; Before I wrote test bellow I thought that my full-pairs worked perfectly well

(define (n-times fn n)
  (define (loop i)
    (fn i)
    (if (< i n)
	(loop (+ i 1))))
  (loop 1))

(define (test-3.67 n)
  ; if there is a bug this test should not stop
  ; (although puny it helped me to find my bug)
  (let ((full-integer-pairs (full-pairs integers integers)))
    (n-times 
     (lambda (x)
       (n-times
	(lambda (y)
	  (debug "x = " x ",\ty = " y "\n")
	  (find-in-stream full-integer-pairs (list x y)))
	n))
     n)))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.68
;;;;;;;;;;;;;;;;;

(define (louis-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x)) t)
   (louis-pairs (stream-cdr s) (stream-cdr t))))

; Aborting!: maximum recursion depth exceeded

; when using cons-stream then evaluation of (interleave ... (pairs ...))
; is delayed and forced only when next element is requested

; Louis implementation does not delay anything 
; it just calls louis-pairs recursively all the time

; in case of infinite streams like integers
; (louis-pairs integers integers) goes into infinite loop

;;;;;;;;;;;;;;;;;
;;; Exercise 3.69
;;;;;;;;;;;;;;;;;

(define (triples s t u)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x)) (pairs t u))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

(define pythagorean-triples
  (stream-filter (lambda (x)
		   (= (+ (square (first x))
			 (square (second x)))
		      (square (third x))))
		 (triples integers integers integers)))

(define (test-3.69)
  ; this is maximum i can do
  (display-stream-n pythagorean-triples 5))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.70
;;;;;;;;;;;;;;;;;

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
	(else
	 (let ((e1 (stream-car s1))
	       (e2 (stream-car s2)))
	   (if (> (weight e1) (weight e2))
	       (cons-stream e2 (merge-weighted s1 (stream-cdr s2) weight))
	       (cons-stream e1 (merge-weighted (stream-cdr s1) s2 weight)))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x)) (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define (test-3.70-a)
  (display-stream-n
   (weighted-pairs integers integers (lambda (x) (apply + x))) 
   50))

(define (test-3.70-b)
  (define (div-2-3-5? x)
    (not (or (= (remainder x 2) 0)
	     (= (remainder x 3) 0)
	     (= (remainder x 5) 0))))
  (define (weight x)
    (let ((i (first x))
	  (j (second x)))
      (+ (* 2 i) (* 3 j) (* 5 i j))))
  (let ((stream-not-div-2-3-5 (stream-filter div-2-3-5? integers)))
    (display-stream-n
     (weighted-pairs stream-not-div-2-3-5 stream-not-div-2-3-5 weight)
     50)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; this is just out of my personal curiosity ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (weighted-triples s t u weight)
  (cons-stream
   (list (stream-car s) (stream-car t) (stream-car u))
   (merge-weighted
    (stream-map 
     (lambda (x) (cons (stream-car s) x)) 
     (weighted-pairs t u weight))
    (weighted-triples (stream-cdr s) (stream-cdr t) (stream-cdr u) weight)
    weight)))

(define weighted-pythagorean-triples
  (stream-filter (lambda (x)
		   (= (+ (square (first x))
			 (square (second x)))
		      (square (third x))))
		 (weighted-triples integers integers integers 
				   (lambda (x) (apply + x)))))

(define (test-3.69-revised)
  ; yeah, baby, yeah! (and it can go over 50)
  (display-stream-n weighted-pythagorean-triples 50))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.71
;;;;;;;;;;;;;;;;;

(define (cubic-sum x)
  (apply + (map cube x)))

(define ordered-by-cubes
  (weighted-pairs integers integers cubic-sum))

(define (filter-equals s test)
  (if (= (test (stream-car s)) (test (stream-car (stream-cdr s))))
      (cons-stream (stream-car s) (filter-equals (stream-cdr s) test))
      (filter-equals (stream-cdr s) test)))

(define ramanujan-numbers
  (stream-map cubic-sum (filter-equals ordered-by-cubes cubic-sum)))

(define (test-3.71)
  (display-stream-n ramanujan-numbers 6))

;; 1729
;; 4104
;; 13832
;; 20683
;; 32832
;; 39312

;;;;;;;;;;;;;;;;;
;;; Exercise 3.72
;;;;;;;;;;;;;;;;;

(define (square-sum x)
  (apply + (map square x)))

(define ordered-by-squares
  (weighted-pairs integers integers square-sum))

(define (filter-triples s test)
  (let ((e1 (stream-car s))
	(e2 (stream-car (stream-cdr s)))
	(e3 (stream-car (stream-cdr (stream-cdr s)))))
    (if (= (test e1) (test e2) (test e3))
	(cons-stream 
	 (list (test e1) e1 e2 e3) 
	 (filter-triples (stream-cdr s) test))
	(filter-triples (stream-cdr s) test))))

(define (test-3.72)
  (display-stream-n (filter-triples ordered-by-squares square-sum) 6))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.73
;;;;;;;;;;;;;;;;;

(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value (add-streams (scale-stream integrand dt) int)))
  int)

(define (RC R C dt)
  (lambda (i v0)
    (add-streams
     (scale-stream i R)
     (scale-stream (integral i v0 dt) (/ 1 C)))))

(assert '(listify-stream-n ((RC 5 1 1) ones 1) 10)
	'(6 7 8 9 10 11 12 13 14 15))

(assert '(listify-stream-n ((RC 5 1 1) integers 1) 10)
	'(6 12 19 27 36 46 57 69 82 96))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.74
;;;;;;;;;;;;;;;;;

(define (streamify l)
  (if (null? l)
      the-empty-stream
      (cons-stream (car l) (streamify (cdr l)))))

; from book:
; That is, the resulting signal should be +1 whenever the input signal
; changes from negative to positive, -1 whenever the input signal changes
; from positive to negative, and 0 otherwise.

; but to complete exercise correctly:
; (sign-change-detector positive negative) -> +1
; (sign-change-detector negative positive) -> -1

; Alyssa's code uses sign-change-detector that is implemented like this

(define (sign-change-detector x y)
  (cond ((and (>= x 0) (< y 0)) 1)
	((and (< x 0) (>= y 0)) -1)
	(else 0)))	 

(define sense-data (streamify '(1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4)))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))

(assert '(listify-stream zero-crossings)
	'(0 0 0 0 0 -1 0 0 0 0 1 0 0))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.75
;;;;;;;;;;;;;;;;;

(define (make-cosine-stream x dx)
  (cons-stream (cos x) (make-cosine-stream (+ x dx) dx)))

(define (jitter strength)
  (- (float-random (* 2 strength)) strength))

(define (add-noise stream strength)
  (cons-stream (+ (stream-car stream) (jitter strength))
	       (add-noise (stream-cdr stream) strength)))

(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))

(define (make-avg-zero-crossings input-stream last-value)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-value)
                 (make-avg-zero-crossings (stream-cdr input-stream)
					  avpt))))

(define (make-avg-zero-crossings-fixed input-stream last-value last-avpt)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-avpt)
                 (make-avg-zero-crossings-fixed (stream-cdr input-stream)
						(stream-car input-stream)
						avpt))))

(define (test-3.75)
  (let ((cosine (make-cosine-stream 0 0.5))
	(print (lambda (x) (debug (listify-stream-n x 30) "\n"))))
    (debug "\nvanilla\n")
    (print (make-zero-crossings cosine 0))
    (debug "\nwith noise\n")
    (let ((noise (add-noise cosine 0.9)))
      (print (make-zero-crossings noise 0))
      (debug "\nwith noise averaged\n")
      (print (make-avg-zero-crossings noise 0))
      (debug "\nwith noise averaged fixed\n")
      (print (make-avg-zero-crossings-fixed noise 0 0))
      (debug "\nwith noise smoothed\n")
      (print (zero-crossing-detector noise 0)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.76
;;;;;;;;;;;;;;;;;

(define (smooth-stream stream last-value)
  (cons-stream (average (stream-car stream) last-value)
	       (smooth-stream (stream-cdr stream) (stream-car stream))))

(define (zero-crossing-detector signal last-value)
  (make-zero-crossings (smooth-stream signal last-value) last-value))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.77
;;;;;;;;;;;;;;;;;

(define (maybe-force x)
  ; cons-stream produces pair in mit-scheme
  (if (pair? x) x (force x)))

(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
		 ; this little trick preserves backward compatibility
                 (let ((integrand (maybe-force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value	       
	       (let ((integrand (maybe-force delayed-integrand)))
		 (if (stream-null? integrand)
		     the-empty-stream
		     (integral (delay (stream-cdr integrand))
			       (+ (* dt (stream-car integrand))
				  initial-value)
			       dt)))))

(assert '(stream-ref (solve (lambda (y) y) 1 0.001) 1000)
	2.716923932235896)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.78
;;;;;;;;;;;;;;;;;

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream y b) (scale-stream dy a)))
  y)
  
(assert '(stream-ref (solve-2nd 2 -1 0.001 1 1) 1000)
	2.716923932235896)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.79
;;;;;;;;;;;;;;;;;

(define (solve-2nd-general f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(assert '(stream-ref (solve-2nd-general average 0.001 1 1) 1000)
	2.716923932235896)

;;;;;;;;;;;;;;;;;
;;; Exercise 3.80
;;;;;;;;;;;;;;;;;

(define (RLC R L C dt)
  (lambda (VC0 IL0)
    (define VC (integral (delay dVC) VC0 dt))
    (define IL (integral (delay dIL) IL0 dt))
    (define dVC (scale-stream IL (/ -1 C)))
    (define dIL (add-streams
		 (scale-stream VC (/ 1 L))
		 (scale-stream IL (/ (- R) L))))
    (cons VC IL)))

(define (test-3.80)
  ; I use zero crossing detector because RLC circuit generates harmonic
  ; oscillations, VC and IL oscillations should be shifted in phase
  (define (print x y)
    (debug "\n" x " = " (listify-stream-n y 30) "\n"))
  (let ((result ((RLC 1 1 0.2 0.2) 10 0)))
    (print "VC" (make-zero-crossings (car result) 0))
    (print "IL" (make-zero-crossings (cdr result) 0))))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.81
;;;;;;;;;;;;;;;;;

(define (reset-random random-init)
  (define seeds (cons-stream random-init (stream-map rand-update seeds)))  
  (stream-map (lambda (x) (remainder x (expt 2 31))) (stream-cdr seeds)))

(define (stream-cadr stream)
  (stream-car (stream-cdr stream)))

(define (make-random request-stream)
  (define (get-random random-stream request-stream)
    (cond ((null? request-stream) the-empty-stream)
	  ((eq? (stream-car request-stream) 'reset)
	   (get-random (reset-random (stream-cadr request-stream))
		       (stream-cdr (stream-cdr request-stream))))
	  ((eq? (stream-car request-stream) 'generate)
	   (cons-stream (stream-car random-stream)
			(get-random (stream-cdr random-stream)
				    (stream-cdr request-stream))))
	  (else (error "MAKE-RANDOM bad value in request stream"
		       (stream-car request-stream)))))	  
  (get-random (reset-random 1) request-stream))

(define (test-3.81 cmd)
  (listify-stream (make-random (streamify cmd))))

(assert '(test-3.81 '(generate generate generate generate))
	'(1103527590 377401575 662824084 1147902781))

(assert '(test-3.81 '(reset 1 generate generate generate generate))
	'(1103527590 377401575 662824084 1147902781))

(assert '(test-3.81 '(reset 100 generate generate generate generate))
	'(829870797 1533044610 1478614675 1357823696))

(assert '(test-3.81 '(reset 100 generate generate reset 100 generate generate))
	'(829870797 1533044610 829870797 1533044610))

;;;;;;;;;;;;;;;;;
;;; Exercise 3.82
;;;;;;;;;;;;;;;;;

(define (random-in-range a b random)
  (let ((range (abs (- a b))))
    (+ (min a b) (* range (/ random (expt 2.0 31))))))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define (test-integral predicate x1 y1 x2 y2)
  (map-successive-pairs 
   (lambda (r1 r2)
     (predicate (random-in-range x1 x2 r1)
		(random-in-range y1 y2 r2)))
   (reset-random 1)))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (estimate-integral predicate x1 y1 x2 y2)
  (stream-map (lambda (s) (* (surface x1 y1 x2 y2) s))	      
	      (monte-carlo (test-integral predicate x1 y1 x2 y2) 0 0)))

(assert
 '(stream-ref (estimate-integral (make-circle 0 0 1) -1.0 -1.0 1.0 1.0) 10000)
 (lambda (x) (< (- pi 0.1) x (+ pi 0.1))))
