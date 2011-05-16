;;;;;;;;;;;;;;;;;
;;; Exercise 2.1
;;;;;;;;;;;;;;;;;

(define (gcd a b)  
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (debug (numer x) "/" (denom x) "\n"))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd (abs n) (abs d))))
    (cons (/ (if (< d 0) (- n) n) g)
	  (/ (abs d) g))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.2
;;;;;;;;;;;;;;;;;

(define (average x y)
  (/ (+ x y) 2))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (midpoint-segment s)
  (let ((p1 (start-segment s))
	(p2 (end-segment s)))
    (make-point (average (x-point p1) (x-point p2))
		(average (y-point p1) (y-point p2)))))

(define (print-point p)
  (debug "(" (x-point p) "," (y-point p) ")\n"))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.3
;;;;;;;;;;;;;;;;;

; rectangle implementation in terms of two corners
;
; p1 *----------
;    |          |
;    |          |
;    |          |
;     ----------* p2
;
(define (make-rectangle p1 p2)
  (cons p1 p2))

(define (rectangle-top-left-corner r)
  (car r))

(define (rectangle-bottom-right-corner r)
  (cdr r))

(define (rectangle-side r axis)
  (abs (- (axis (rectangle-top-left-corner r))
	  (axis (rectangle-bottom-right-corner r)))))

(define (x-side r)
  (rectangle-side r x-point))

(define (y-side r)
  (rectangle-side r y-point))

(define (perimeter r)
  (* 2 (+ (x-side r) (y-side r))))

(define (area r)
  (* (x-side r) (y-side r)))

; rectangle implementation in terms of point and side lengths
;
; p  *----------
;    |          |
;    |          | s.y
;    |          |
;     ----------
;        s.x
;
(define (make-rectangle p s)
  (cons p s))

(define (rectangle-side-length r)
  (cdr r))

(define (rectangle-side r axis)
  (abs (axis (rectangle-side-length r))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.4
;;;;;;;;;;;;;;;;;

(define (cons-2 x y)
  (lambda (m) (m x y)))

(define (car-2 z)
  (z (lambda (p q) p)))

(define (cdr-2 z)
  (z (lambda (p q) q)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.5
;;;;;;;;;;;;;;;;;

(define (cons-3 x y)
  (* (fast-expt 2 x)
     (fast-expt 3 y)))
  
(define (get-powers z p)
  (if (not (= 0 (remainder z p)))
      0
      (+ 1 (get-powers (/ z p) p))))

(define (car-3 z)
  (get-powers z 2))

(define (cdr-3 z)
  (get-powers z 3))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.6
;;;;;;;;;;;;;;;;;

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; 0 (lambda (f) (lambda (x) x))

; 1 (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) 
;                                                            f) x))))

; 2 (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) 
;                           (f (((lambda (f) (lambda (x) x)) f) x))))
;                                                            f) x))))

; 3 (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) 
;                           (f (((lambda (f) (lambda (x) 
;                           (f (((lambda (f) (lambda (x) x)) f) x))))
;                                                            f) x)))) 
;                                                            f) x))))


(define three (add-1 (add-1 (add-1 zero))))

(define four (add-1 (add-1 (add-1 (add-1 zero)))))

(define five (add-1 (add-1 (add-1 (add-1 (add-1 zero))))))

(define (eval-num n)
  ((n inc) 0))

(define (plus n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

; (eval-num (plus five (plus three four))) ; should yield 12

;;;;;;;;;;;;;;;;;
;;; Exercise 2.7
;;;;;;;;;;;;;;;;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (reduce-list fn e l)
  (if (eq? l '())
      e
      (reduce-list fn (fn e (car l)) (cdr l))))

(define (min-many e . l)
  (reduce-list min e l))

(define (max-many e . l)
  (reduce-list max e l))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min-many p1 p2 p3 p4)
                   (max-many p1 p2 p3 p4))))

(define (reciprocal-interval i)
  (make-interval
   (/ 1.0 (upper-bound i))
   (/ 1.0 (lower-bound i))))

(define (div-interval x y)
  (mul-interval x (reciprocal-interval y)))

(define (make-interval a b)
  (cons a b))

(define (lower-bound i)
  (car i)) ; consider (min (car i) (cdr i)))

(define (upper-bound i)
  (cdr i)) ; consider (max (car i) (cdr i)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.8
;;;;;;;;;;;;;;;;;

(define (inv-interval i)
  (make-interval
   (- (upper-bound i))
   (- (lower-bound i))))

(define (sub-interval a b)
  (add-interval a (inv-interval b)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.9
;;;;;;;;;;;;;;;;;

; for intervals i1 and i2, w1 = (i1.e - i1.s)/2 and w2 = (i1.e - i1.s)/2
; i3 = (add-interval i1 i2), where i3.s = i1.s + i2.s and i3.e = i1.e + i2.e
; so width of i3
; = ((i1.e + i2.e) - (i1.s + i2.s))/2
; = i1.e/2 + i2.e/2 - i1.s/2 - i2.s/2
; = (i1.e - i1.s)/2 + (i1.e - i1.s)/2
; = w1 + w2

; the same holds true for substraction
; because inv-interval does not change the width of interval

; > (mul-interval (make-interval 1 2) (make-interval 2 3))
; (2 . 6)
; > (mul-interval (make-interval 5 6) (make-interval 6 7))
; (30 . 42)

; width of input intervals in all cases are 1/2,
; first expression yields output interval of width 2
; while second expression's output interval is of width 6

; > (div-interval (make-interval 1 2) (make-interval 2 3))
; (0.3333333333333333 . 1.0) ; width 0.333
; > (div-interval (make-interval 5 6) (make-interval 2 3))
; (1.6666666666666665 . 3.0) ; width 0.666

; same reasoning as with multiplication

;;;;;;;;;;;;;;;;;
;;; Exercise 2.10
;;;;;;;;;;;;;;;;;

(define (signum x)
  (cond ((> x 0) 1)
	((< x 0) -1)
	(else 0)))

(define (reciprocal-interval i)
  (let ((l (lower-bound i))
	(u (upper-bound i)))
    (if (or (= 0 l u) (not (= (signum l) (signum u))))
	(error "divide by interval that spans zero")
	(make-interval
	 (/ 1.0 (upper-bound i))
	 (/ 1.0 (lower-bound i))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.11
;;;;;;;;;;;;;;;;;

; knowing that on modern machines conditionals are more expensive
; than multiplication makes this exercise very uninteresting

(define (/= x y)
  (not (= x y)))

(define (eq-interval a b)
  (and (= (lower-bound a) (lower-bound b))
       (= (upper-bound a) (upper-bound b))))

; I made up fictional nine cases for variables lx ly ux uy

; 1. all greater than zero
; 2. all greater than zero, but lx is zero
; 3. all greater than zero, but ly is zero
; 4. ux and uy is greater than zero, lx and ly is zero
; 5. all less than zero
; 6. all less than zero, but ux is zero
; 7. all less than zero, but uy is zero
; 8. lx and ly is less than zero, ux and uy is zero
; 9. signs differ

(define (less-mul-interval x y)
  (let ((lx (lower-bound x))
	(ux (upper-bound x))
 	(ly (lower-bound y))
 	(uy (upper-bound y)))
    (let ((m1 (min-many lx ux ly uy))
	  (m2 (max-many lx ux ly uy)))
      (cond ((= m1 0) (make-interval 0 (* ux uy)))		; 2,3,4
	    ((> m1 0) (make-interval (* lx ly) (* ux uy)))	; 1
	    ((= m2 0) (make-interval 0 (* lx ly)))		; 5,6,7
	    ((< m2 0) (make-interval (* ux uy) (* lx ly)))	; 8
	    (else (mul-interval x y))))))			; 9

(define (test-single-interval a b)
  (let ((r1 (mul-interval a b))
	(r2 (less-mul-interval a b)))
    (cond ((not (eq-interval r1 r2))
	   (debug "i1=" a " \ti2=" b " \tr1=" r1 " \tr2=" r2 "\n")))))

(define (random-interval)
  (let ((x (- (random 10) 5)))
    (make-interval x (+ x (random 10)))))

(define (test-intervals n)
  (test-single-interval (random-interval) (random-interval))
  (cond ((> n 0) (test-intervals (- n 1)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.12
;;;;;;;;;;;;;;;;;

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
  (let ((w (* 0.01 p c)))
    (make-interval (- c w) (+ c w))))

(define (percent i)
  (/ (width i) (center i) 0.01))
     
;;;;;;;;;;;;;;;;;
;;; Exercise 2.13
;;;;;;;;;;;;;;;;;

; claim:
;  if intervals a * b = c, then
;  (percent a) + (percent b) ~ (percent c) for small percentage tolerances

;  interval x can be described as 
;  c(x) +/- c(x) * q(x) = c(x) * (1 +/- q(x))
;  where q(x) = (percent x) and c(x) = (center x)

;  then in equation a * b = c,
;  c.lower_bound = c(a) * (1 - q(a)) * c(b) * (1 - q(b))
;  c.upper_bound = c(a) * (1 + q(a)) * c(b) * (1 + q(b))
;  (percent c) = (width c) / (center c)

;  (percent c) = (c(a)*(1 + q(a)) * c(b)*(1 + q(b)) / 2 
;               - c(a)*(1 - q(a)) * c(b)*(1 - q(b)) / 2)
;              / (c(a)*(1 + q(a)) * c(b)*(1 + q(b)) / 2 
;               + c(a)*(1 - q(a)) * c(b)*(1 - q(b)) / 2)

;  (percent c) = ((c(a) + c(a)*q(a)) * (c(b) + c(b)*q(b))
;               - (c(a) - c(a)*q(a)) * (c(b) - c(b)*q(b))
;	       / ((c(a) + c(a)*q(a)) * (c(b) + c(b)*q(b))
;               + (c(a) - c(a)*q(a)) * (c(b) - c(b)*q(b)))

;  (percent c) =
;       (c(a)*c(b) + c(a)*q(a)*c(b) + c(a)*c(b)*q(b) + c(a)*q(a)*c(b)*q(b)
;      - c(a)*c(b) + c(a)*q(a)*c(b) + c(a)*c(b)*q(b) - c(a)*q(a)*c(b)*q(b))
;     / (c(a)*c(b) + c(a)*q(a)*c(b) + c(a)*c(b)*q(b) + c(a)*q(a)*c(b)*q(b)
;      + c(a)*c(b) - c(a)*q(a)*c(b) - c(a)*c(b)*q(b) + c(a)*q(a)*c(b)*q(b))

;  ... tum ti tum, some simplification ...

;  (percent c) = ( q(a) + q(b) ) / ( 1 + q(a) * q(b) )
;  for small q(a) and q(b), their product q(a)*q(b) is miniscule

;;;;;;;;;;;;;;;;;
;;; Exercise 2.14
;;;;;;;;;;;;;;;;;

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1))) 
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

; > (define a (make-center-percent 10 2))
; > (define b (make-center-percent 20 1))

; > (par1 a b)
; (6.382894736842106 . 6.960810810810809)
; > (par2 a b)
; (6.555405405405406 . 6.777631578947367)

; > (percent (par1 a b))
; 4.3310013991604865
; > (percent (par2 a b))
; 1.6667333466693208

;;;;;;;;;;;;;;;;;
;;; Exercise 2.15
;;;;;;;;;;;;;;;;;

; Eva Lu Ator is right (for example look results of previous exercise)
; every operation where both terms is uncertain numbers ( width(x) > 0 )
; increases width of result to be larger than width of each of terms
; in par1 there is 3 such operations, while in par2 there is just 1

;;;;;;;;;;;;;;;;;
;;; Exercise 2.16
;;;;;;;;;;;;;;;;;

; I think to devise an interval-arithmetic package that does not have
; equivalent algebraic expressions that lead to different answers, one must
; first simplify expression to some canonical form and only then evaluate

;;;;;;;;;;;;;;;;;
;;; Exercise 2.17
;;;;;;;;;;;;;;;;;

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.18
;;;;;;;;;;;;;;;;;

(define (reverse x)
  (define (reverse-build x result)
    (if (null? x)
	result
	(reverse-build (cdr x) (cons (car x) result))))
  (reverse-build x '()))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.19
;;;;;;;;;;;;;;;;;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define no-more? null?)
(define first-denomination car)
(define except-first-denomination cdr)

; order of coin values does not affect answer, because
; it does not matter in what order we give out particular sets of coins
; e.g. changes for 60 -> (50 10) and (10 50) are the same combination

;;;;;;;;;;;;;;;;;
;;; Exercise 2.20
;;;;;;;;;;;;;;;;;

(define (parity x)
  (remainder x 2))

(define (same-parity-of-rest x y)  
  (cond ((null? y) y)
	((= (parity x) (parity (car y))) 
	 (cons (car y) (same-parity-of-rest x (cdr y))))
	(else (same-parity-of-rest x (cdr y)))))

(define (same-parity x . y)
  (cons x (same-parity-of-rest x y)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.21
;;;;;;;;;;;;;;;;;

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items))
	    (square-list (cdr items)))))

(define (square-list items)
  (map square items))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.22
;;;;;;;;;;;;;;;;;

; *) because iterative process takes elements from the begining of the list
;    (a1 a2 a3 a4) and builds list starting from end:
;           1) (cons a1 nil)
;           2) (cons a2 (cons a1 nil))
;           3) (cons a3 (cons a2 (cons a1 nil)))
;           4) (cons a4 (cons a3 (cons a2 (cons a1 nil))))
;
; *) Louis fix is even weirder ((((nil . a1) . a2) . a3) . a4)
;           1) (cons nil a1)
;           2) (cons (cons nil a1) a2)
;           3) (cons (cons (cons nil a1) a2) a3)
;           4) (cons (cons (cons (cons nil a1) a2) a3) a4)

; Explanation: Louis Reasoner is stupid!

;;;;;;;;;;;;;;;;;
;;; Exercise 2.23
;;;;;;;;;;;;;;;;;

(define (for-each fn list)
  (cond ((null? list) #t)
	(else
	 (fn (car list))
	 (for-each fn (cdr list)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.24
;;;;;;;;;;;;;;;;;

; (1 (2 (3 4)))

;  [1|*]-->[*|/]
;           |
;           |
;          [2|*]-->[*|/]
;                   |
;                   |
;                  [3|*]-->[4|/]

;;;;;;;;;;;;;;;;;
;;; Exercise 2.25
;;;;;;;;;;;;;;;;;

; > (car (cdaddr '(1 3 (5 7) 9)))
; 7

; > (caar '((7)))
; 7

; > (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7)))))))))
; 7

;;;;;;;;;;;;;;;;;
;;; Exercise 2.26
;;;;;;;;;;;;;;;;;

; > (append x y)
; (1 2 3 4 5 6)
; > (cons x y)
; ((1 2 3) 4 5 6)
; > (list x y)
; ((1 2 3) (4 5 6))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.27
;;;;;;;;;;;;;;;;;

(define (deep-reverse x)
  (define (reverse-build x result)
    (cond ((null? x) result)
	  ((pair? (car x)) 
	   (reverse-build (cdr x) (cons (deep-reverse (car x)) result)))
	  (else (reverse-build (cdr x) (cons (car x) result)))))
  (reverse-build x '()))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.28
;;;;;;;;;;;;;;;;;

(define (fringe x)
  (cond ((null? x) x)
	((not (pair? x)) (list x))
	(else (append (fringe (car x)) (fringe (cdr x))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.29
;;;;;;;;;;;;;;;;;

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

; A.
(define left-branch car)
(define right-branch cadr)

(define branch-length car)
(define branch-structure cadr)

; B.
(define (branch-weight branch)
  (let ((structure (branch-structure branch)))
    (if (pair? structure)
	(total-weight structure)
	structure)))

(define (total-weight binary-mobile)
  (+ (branch-weight (left-branch binary-mobile))
     (branch-weight (right-branch binary-mobile))))

; C.

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (float-eq? x y)
  (< (abs (- x y)) 0.000000000001))

(define (is-balanced binary-mobile)
  (cond ((not (pair? binary-mobile)) #t) ; just a weight
	(else 
	 (and (is-balanced (branch-structure (left-branch binary-mobile)))
	      (is-balanced (branch-structure (right-branch binary-mobile)))
	      (float-eq? (torque (left-branch binary-mobile))
			 (torque (right-branch binary-mobile)))))))

; D.
(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

(define left-branch car)
(define right-branch cadr)

; I need to change two these two lines
(define right-branch cdr)
(define branch-structure cdr)

(define test-mobile-1 ; total weight 24
  (make-mobile 
   (make-branch 1 (make-mobile
		   (make-branch 1 2)
		   (make-branch 1 1)))
   (make-branch 2 (make-mobile
		   (make-branch 3 6)
		   (make-branch 4 (make-mobile
				   (make-branch 5 7)
				   (make-branch 6 8)))))))

(define test-mobile-2 ; balanced
  (make-mobile
   (make-branch 1 2)
   (make-branch 2 (make-mobile
		   (make-branch 0.6 0.4)
		   (make-branch 0.4 0.6)))))

(define (float-random float)
  (* float (/ (random 10000) 10000.0)))

(define (gen-simple-balanced weight)
  (let ((x (float-random weight)))
    (make-mobile
     (make-branch x (- weight x))
     (make-branch (- weight x) x))))

(define (gen-balanced-branch length weight)
  (make-branch
   length
   (if (or (< weight 1.0) (= 0 (random 2)))
       (gen-simple-balanced weight)
       (gen-complex-balanced weight))))

(define (gen-complex-balanced weight)
  (let ((x (float-random 1.0)))
    (make-mobile
     (gen-balanced-branch (- 1.0 x) (* x weight))
     (gen-balanced-branch x (* (- 1.0 x) weight)))))

; (is-balanced (gen-complex-balanced x)) ; should return #t
; (total-weight (gen-complex-balanced x)) ; should return x

;;;;;;;;;;;;;;;;;
;;; Exercise 2.30
;;;;;;;;;;;;;;;;;

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (square-tree-2 tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-2 (car tree))
                    (square-tree-2 (cdr tree))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.31
;;;;;;;;;;;;;;;;;

(define (tree-map fn tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fn sub-tree)
             (fn sub-tree)))
       tree))

(define (tree-map-2 tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (fn tree))
        (else (cons (tree-map-2 fn (car tree))
                    (tree-map-2 fn (cdr tree))))))

(define (square-tree-3 tree) (tree-map square tree))

(define (square-tree-4 tree) (tree-map-2 square tree))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.32
;;;;;;;;;;;;;;;;;

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

; this can be proven by induction
; imagine that set of elements are (e1, e2, ... en)
; we can show that (subsets s) works correctly for nil and for one element set
; then we assume that (subsets s) works for set of n-1 elements
; then evaluating (subsets s) of set that has n elements
; code puts "correct" answer in rest variable due to the assumtion,
; because (subsets (cdr s)) is set of size n-1,
; then we unite rest with copy of rest where 
; each subset is united with nth element,
; thus yielding complete set of subsets of size n

;;;;;;;;;;;;;;;;;
;;; Exercise 2.33
;;;;;;;;;;;;;;;;;

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map-2 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
; this map is intersting, it returns correct result if everything is 
; purely functional, but has problems with functions that have side effects,
; because it does not map from left to right as I was expecting

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.34
;;;;;;;;;;;;;;;;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) 
		(+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.35
;;;;;;;;;;;;;;;;;

(define (count-leaves t)
  (accumulate + 0 (map (lambda (x) (if (pair? x) (count-leaves x) 1)) t)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.36
;;;;;;;;;;;;;;;;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.37
;;;;;;;;;;;;;;;;;

(define (multi-map fn . lists)
  (if (null? (car lists))
      '()
      (cons (apply fn (map car lists))
	    (apply multi-map (cons fn (map cdr lists))))))

(define (dot-product v w)
  (accumulate + 0 (multi-map * v w)))
(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
  (accumulate-n cons '() mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector n x)) m)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.38
;;;;;;;;;;;;;;;;;

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

; > (fold-right / 1 (list 1 2 3))
; 3/2
; > (fold-left / 1 (list 1 2 3))
; 1/6
; > (fold-right list '() (list 1 2 3))
; (1 (2 (3 ())))
; > (fold-left list '() (list 1 2 3))
; (((() 1) 2) 3)

; in order for fold-left and fold-right to return same results,
; op must be ASSOCIATIVE opperation

;;;;;;;;;;;;;;;;;
;;; Exercise 2.39
;;;;;;;;;;;;;;;;;

(define (reverse-1 sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))
(define (reverse-2 sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.40
;;;;;;;;;;;;;;;;;

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (listify x)
  (if (not (pair? x))
      (list x)
      x))

(define (last x)
  (cond ((not (pair? x)) x)
	((null? (cdr x)) (car x))
	(else (last (cdr x)))))

(define (generate-pairs-for i)
  ; input: a		output: ((a 1) (a 2) ... (a a-1))
  ; input: (a b c)	output: ((a b c 1) (a b c 2) ... (a b c c-1))
  (map (lambda (j) (append (listify i) (list j)))
       (enumerate-interval 1 (- (last i) 1))))

(define (unique-pairs n)
  (flatmap generate-pairs-for (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.41
;;;;;;;;;;;;;;;;;

(define (generate-triples n)
  (flatmap generate-pairs-for (unique-pairs n)))

(define (triples-of-sum n s)
  (define (good-sum? x)
    (= s (accumulate + 0 x)))
  (filter good-sum? (generate-triples n)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.42
;;;;;;;;;;;;;;;;;

(define empty-board nil)

(define (adjoin-position new-row column rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (is-safe? row1 k rowk)
  (and (not (= row1 rowk))
       (not (= (abs (- rowk row1))
	       (- k 1)))))

(define (safe? k positions)
  (or (= k 1)
      (and (is-safe? (car positions) k (list-ref positions (- k 1)))
	   (safe? (- k 1) (cdr positions)))))

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

(define (print-board configuration)
  (let ((size (length configuration)))
    (define (print-line x)
      (cond ((= x 0) (newline))	 
	    (else (display (if (odd? x) "+" "-"))
		  (print-line (- x 1)))))
    (define (print-size-line)
      (print-line (+ 1 (* size 2))))
    (define (print-cell x y)
      (display (if (= (inc y) (list-ref configuration x)) "*" " ")))
    (define (print-row-of-cells x y)
      (cond ((not (= x size))
	     (display "|")
	     (print-cell x y)
	     (print-row-of-cells (inc x) y))
	    (else (display "|\n"))))
    (define (print-next-row y)
      (cond ((not (= y size))
	     (print-size-line)
	     (print-row-of-cells 0 y)
	     (print-next-row (inc y)))
	    (else (print-size-line))))
    (print-next-row 0)
    (newline)))

(define (pretty-queens n)
  (map print-board (queens n))
  (display ""))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.43
;;;;;;;;;;;;;;;;;

(define (louis-queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
	 (flatmap
	  (lambda (new-row)
	    (map (lambda (rest-of-queens)
		   (adjoin-position new-row k rest-of-queens))
		 (queen-cols (- k 1))))
	  (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

; louis programms runs slow because for for kth queen
; (queen...) algorithm calls (queen-cols (- k 1)) once, 
; while (louis-queen...) calls (queen-cols (- k 1)) board-size times
; comparing to (queen...) which calls enumerate-interval several times
; (louis-queen...) calls enumerate-interval only once, but
; complexity of enumerate-interval is only O(n), while queen-cols has
; much greater complexity
;
; n | 100 * queens | louis-queens |
;---+--------------+--------------+
; 4 |    0.004 s   |   0.001 s    |
; 5 |    0.017 s   |   0.004 s    |
; 6 |    0.085 s   |   0.081 s    |
; 7 |    0.412 s   |   1.458 s    |
; 8 |    2.021 s   |  32.871 s    |
;
; if (qeens 8) runs for time T, then louis-queens will run for ~ 1600 * T

;;;;;;;;;;;;;;;;;
;;; Exercise 2.53
;;;;;;;;;;;;;;;;;

;> (list 'a 'b 'c)
; (a b c)
;> (list (list 'george))
;((george))
;> (cdr '((x1 x2) (y1 y2)))
;((y1 y2))
;> (cadr '((x1 x2) (y1 y2)))
;(y1 y2)
;> (pair? (car '(a short list)))
;#f
;> (memq 'red '((red shoes) (blue socks)))
;#f
;> (memq 'red '(red shoes blue socks))
;(red shoes blue socks)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.54
;;;;;;;;;;;;;;;;;

(define (equal?-2 x y)
  (cond ((and (not (pair? x)) 
	      (not (pair? y))
	      (eq? x y)))
	((and (pair? x)
	      (pair? y)
	      (equal?-2 (car x) (car y))
	      (equal?-2 (cdr x) (cdr y))))
	(else #f)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.55
;;;;;;;;;;;;;;;;;

; ''abracadabra is '(quote abracadabra)
; and it evaluates to list of two symbols (quote abracadabra)
; car of that list is symbol "quote"

;;;;;;;;;;;;;;;;;
;;; Exercise 2.56
;;;;;;;;;;;;;;;;;

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) 'expt)))

(define (base s) (cadr s))

(define (exponent s) (caddr s))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list 'expt base exponent))))

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
         (make-product
	  (make-product
	   (exponent exp)
	   (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
	  (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIV" exp))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.57
;;;;;;;;;;;;;;;;;

(define (augend s) 
  (if (not (null? (cdddr s)))
      (cons '+ (cddr s))
      (caddr s)))
      
(define (multiplicand p) 
  (if (not (null? (cdddr p)))
      (cons '* (cddr p))
      (caddr p)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.58A
;;;;;;;;;;;;;;;;;

(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2)))) ; ***

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2)))) ; ***

(define (exponentiation? x)
  (and (pair? x) (eq? (cadr x) '^)))

(define (base s) (car s))

(define (make-exponentiation base exponent)
  (cond ((=number? base 1) 1)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list base '^ exponent)))) ; ***

;;;;;;;;;;;;;;;;;
;;; Exercise 2.58B
;;;;;;;;;;;;;;;;;

(define (check-op x op)
  (and (pair? x) (memq op x)))

(define (sum? x)
  (check-op x '+))

; it is not necessary to check (not (sum? x)) because deriv does it for us
(define (product? x)
  (check-op x '*))

(define (chop-to-next s x)
  (if (not (eq? x (car s)))
      (cons (car s) (chop-to-next (cdr s) x))
      '()))

(define (check-tail-exp x)
  (if (null? (cddr x))
      (cadr x)
      (cdr x)))

(define (first-term s op)
  (if (not (eq? op (cadr s)))
      (chop-to-next s op)
      (car s)))

(define (second-term s op)
  (check-tail-exp
   (if (not (eq? op (cadr s)))
       (memq op s)
       (cdr s))))

(define (addend s)
  (first-term s '+))

(define (augend s)
  (second-term s '+))

; at this point we can be sure that there is no '+ in s
(define (multiplier s)
  (first-term s '*))

(define (multiplicand s)
  (second-term s '*))

(define (tail-after-last s l)
  (reverse (cdr (memq s (reverse l)))))

(define (simplify-elt x)
  (if (and (pair? x) (null? (cdr x)))
      (car x)
      x))

(define (base s)
  (simplify-elt (tail-after-last '^ s)))

(define (exponent s)
  (let ((rest (memq '^ s)))
    (and rest (simplify-elt (or (exponent (cdr rest)) (cdr rest))))))

; NOTE:
; exponentation implementation is wrong
; x ^ y ^ z is assumbed to be: (x ^ y) ^ z, while it should be: x ^ (y ^ z)
; right solution is to leave exponentation as it is from previous exercise

; x ^ 6 drived is 6 * x ^ 5
(assert '(deriv '(x ^ 2 ^ 3) 'x) '((3 * ((x ^ 2) ^ 2)) * (2 * x)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.59
;;;;;;;;;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)        
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (adjoin-set (car set1) set2))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.60
;;;;;;;;;;;;;;;;;

(define (adjoin-set x set) ;; constant (was linear)
  (cons x set))

(define (union-set set1 set2) ;; linear (was quadratic)
  (append set1 set2))

; sets would definitively be larger thus element-of-set? and intersection-set
; will work even slower but still they have same time complexity, on the
; other hand adjoin-set and union-set has now better time complexities
; so this approach could be used in applications where 
; adjoin-set and union-set are called frequently,
; but element-of-set? and intersection-set rarely

;;;;;;;;;;;;;;;;;
;;; Exercise 2.61
;;;;;;;;;;;;;;;;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set-ol set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2) (cons x1 (intersection-set-ol (cdr set1) (cdr set2))))
              ((< x1 x2) (intersection-set-ol (cdr set1) set2))
              ((< x2 x1) (intersection-set-ol set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.62
;;;;;;;;;;;;;;;;;

(define (union-set-ol set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2) (cons x1 (union-set-ol (cdr set1) (cdr set2))))
		      ((< x1 x2) (cons x1 (union-set-ol (cdr set1) set2)))
		      ((< x2 x1) (cons x2 (union-set-ol set1 (cdr set2)))))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.63
;;;;;;;;;;;;;;;;;

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
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

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define test1 (make-tree 7 
			 (make-tree 3 
				    (make-tree 1 '() '())
				    (make-tree 5 '() '()))
			 (make-tree 9 '() (make-tree 11 '() '()))))

(define test2 (make-tree 3
			 (make-tree 1 '() '())
			 (make-tree 7
				    (make-tree 5 '() '())
				    (make-tree 9 '() (make-tree 11 '() '())))))

(define test3 (make-tree 5 
			 (make-tree 3 (make-tree 1 '() '()) '())
			 (make-tree 9
				    (make-tree 7 '() '())
				    (make-tree 11 '() '()))))

; A. 
; procedures produces same results,
; for figure 2.16 - (1 3 5 7 9 11)

; B.
;
; tree->list-2 is O(n) - simply walk the tree, gather all elements
; tree->list-1 is O(n^2) - because walking tree is linear, 
; but for each node we make append which is linear too
; for example in a tree:
;
;         1
;        /
;       2
;      /
;     3
;    /
;   4
;  /
; 5
;
; (append '() '())
; (append '(1) '())
; (append '(1 2) '())
; (append '(1 2 3) '())
; (append '(1 2 3 4) '()))
; ...

; NOTE:
; answer B is not quite right because it was asked about balanced tree

;;;;;;;;;;;;;;;;;
;;; Exercise 2.64
;;;;;;;;;;;;;;;;;

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
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

; A. 
;            5
;           / \
;          /   \
;         /     \
;        1       9
;         \     / \
;          3   7  11

; partial-tree procedure recursively calls self with same list, but
; twice as less n to get left sub-tree ir car, and list for right tree in cdr
; then one element is taken of of cdr for the entry and rest of elements
; ar passed to partial-tree to recursively form right sub-tree
; in the end results are combined with make-tree

; B.
; algorithm is O(n), because it combines n elements by halving
; 2n steps ~ 2 + 4 + 8 + ... + ~n/8 + ~n/4 + ~n/2 + n
; in first step we split whole into two
; in second step each of two is split into two which makes four
; up until we get down to single elements which is n

; actually I checked that partial-tree is called 2n times and make-tree n times
; also (length elements) in list->tree is O(n)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.65
;;;;;;;;;;;;;;;;;

(define (union-set-tree set1 set2)
  (list->tree			; O(n)
   (union-set-ol		; O(n)
    (tree->list-2 set1)		; O(n)
    (tree->list-2 set2))))	; O(n)

(define (intersection-set-tree set1 set2)
  (list->tree			; O(n)
   (intersection-set-ol		; O(n)
    (tree->list-2 set1)		; O(n)
    (tree->list-2 set2))))	; O(n)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.66
;;;;;;;;;;;;;;;;;

(define key car)

(define (lookup given-key records)
  (if (null? records)
      false
      (let ((this-key (key (entry records))))
	(cond	
	 ((= this-key given-key) (entry records))
	 ((> this-key given-key) (lookup given-key (left-branch records)))
	 ((< this-key given-key) (lookup given-key (right-branch records)))))))

; > (lookup 2 (list->tree '((1 andy) (2 sandy) (3 kim))))
; (2 sandy)
; > (lookup 4 (list->tree '((1 andy) (2 sandy) (3 kim))))
; #f

;;;;;;;;;;;;;;;;;
;;; Exercise 2.67
;;;;;;;;;;;;;;;;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (the-symbols left) (the-symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (the-symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

; > (decode sample-message sample-tree)
; (A D A B B C A)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.68
;;;;;;;;;;;;;;;;;

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (choose-bit symbol tree)
  (if (memq symbol (the-symbols (left-branch tree)))
      (cons 0 (encode-symbol symbol (left-branch tree)))
      (cons 1 (encode-symbol symbol (right-branch tree)))))

(define (encode-symbol symbol tree)
  (cond ((not (leaf? tree)) (choose-bit symbol tree))
	((equal? symbol (symbol-leaf tree)) '())
	(else
	 (debug "symbol=" symbol "\n")
	 (error "bad symbol -- ENCODE-SYMBOL"))))

; > (encode '(A D A B B C A) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.69
;;;;;;;;;;;;;;;;;

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs) ; fails when pairs are '()
  (if (null? (cdr pairs))
      (car pairs)
      (successive-merge 
       (adjoin-set (make-code-tree (car pairs) (cadr pairs))
		   (cddr pairs)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.70
;;;;;;;;;;;;;;;;;

(define song-stats 
  '((a 2) (na 16) (boom 1) (sha 3) (get 2) (yip 9) (job 2) (wah 1)))

(define song
  '(get a job
	sha na na na na na na na na
	get a job
	sha na na na na na na na na
	wah yip yip yip yip yip yip yip yip yip
	sha boom))

; > (length (encode song (generate-huffman-tree song-stats)))
; 84

; eight symbol alphabet log2(8) = 3bits
; > (* 3 (length song))
; 108

;;;;;;;;;;;;;;;;;
;;; Exercise 2.71
;;;;;;;;;;;;;;;;;

(define alpha-5
  '((a 1) (b 2) (c 4) (d 8) (e 16)))

(define alpha-10
  '((a 1) (b 2) (c 4) (d 8) (e 16) (f 32) (g 64) (h 128) (i 256) (j 512)))

(define (collect message tree)
  (if (null? message)
      '()
      (cons (encode-symbol (car message) tree)
	    (collect (cdr message) tree))))

(define word-5
  '(a b c d e))

; > (collect word-5 (generate-huffman-tree alpha-5))
; ((0 0 0 0)
;  (0 0 0 1)
;  (0 0 1)
;  (0 1)
;  (1))

(define word-10
  '(a b c d e f g h i j))

; (collect word-10 (generate-huffman-tree alpha-10))
; ((0 0 0 0 0 0 0 0 0)
;  (0 0 0 0 0 0 0 0 1)
;  (0 0 0 0 0 0 0 1)
;  (0 0 0 0 0 0 1)
;  (0 0 0 0 0 1)
;  (0 0 0 0 1)
;  (0 0 0 1)
;  (0 0 1)
;  (0 1)
;  (1))

; to encode most frequent symbol we need 1 bit
; to encode least frequent symbol we need n-1 bit

; because tree would look like
;       /\
;        /\
;         /\
;          /\
;           ...
;            /\

; NOTE:
; this is wrong, tree would go left
;       /\
;      /\
;     /\

;;;;;;;;;;;;;;;;;
;;; Exercise 2.72
;;;;;;;;;;;;;;;;;

; for alphabet size n
; we can encode most frequent symbol of ex2.70 alphabet in O(1) steps
; to encode least frequent symbol we need n steps to go down the tree and
; n steps to search symbol set at each mode, because I use memq,
; that results in O(n^2) to encode least frequent symbol bit. 

; to encode word of size m it would take O(m) 
; if word consisted only from most frequent symbol

; to encode word of size m it would take O(m*n*n^2)
; if word consisted only from least frequent symbol

; to encode word of size m it would take O(m*log(n)*n*log(n))
; if tree were constructed from symbols that all
; appeared in word with equal frequency
; n*log(n) - searching symbol sets * going down the tree
; log(n) - encoded size of symbol

; extra n in this equation comes from append, but it is only my speculation

; in scheme you can not explicitly modify list components like in common-lisp,
; for example:
; (setf (car some-list) 5) works in CL
; (set! (car some-list) 5) does not work in scheme
; therefore I presume scheme append just copies first list and attaches
; second list to the copy of first, so complexity is O(size-of-first-list)
; in common-lisp append copies both lists so complexity is
; O(size-of-first-list + size-of-second-list)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.73
;;;;;;;;;;;;;;;;;

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; A.
; we can't assimilate predicates number? and variable? because 
; they check type of object rather than value of object,
; things can be "put in" and "looked up" in table only by value of some key

; B., C.
(define the-table nil)

(define (get-item id entry)
  (cond ((null? entry) nil)
	((equal? id (caar entry)) (cdar entry))
	(else (get-item id (cdr entry)))))

(define (is-null? list)
  (if (null? list) false list))

(define (get op type)
  (let ((entry (get-item type the-table)))
    (and (pair? entry) (is-null? (get-item op entry)))))

(define (put-item id item entry)
  (cond ((null? entry) (list (cons id item)))
	((equal? id (caar entry)) (cons (cons id item) (cdr entry)))
	(else (cons (car entry) (put-item id item (cdr entry))))))

(define (put op type item)
  (let ((entry (get-item type the-table)))
    (let ((modified-entry (put-item op item entry)))
      (set! the-table (put-item type modified-entry the-table)))))

(define (ex-2.73)
  (define (addend s) (car s))
  (define (augend s) (cadr s))
  (define (multiplier p) (car p))  
  (define (multiplicand p) (cadr p))
  (define (base s) (car s))
  (define (exponent s) (cadr s))
  (define (=number? exp num)
    (and (number? exp) (= exp num)))
  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
	  ((=number? a2 0) a1)
	  ((and (number? a1) (number? a2)) (+ a1 a2))
	  (else (list '+ a1 a2))))
  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	  ((=number? m1 1) m2)
	  ((=number? m2 1) m1)
	  ((and (number? m1) (number? m2)) (* m1 m2))
	  (else (list '* m1 m2))))
  (define (make-exponentiation base exponent)
    (cond ((=number? base 1) 1)
	  ((=number? exponent 0) 1)
	  ((=number? exponent 1) base)
	  ((and (number? base) (number? exponent)) (expt base exponent))
	  (else (list 'expt base exponent))))
  (define (sum exp var)
    (make-sum (deriv (addend exp) var)
	      (deriv (augend exp) var)))
  (define (product exp var)
    (make-sum
     (make-product (multiplier exp) (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var) (multiplicand exp))))
  (define (exponentiation exp var)
    (make-product
     (make-product
      (exponent exp)
      (make-exponentiation (base exp) (make-sum (exponent exp) -1)))
     (deriv (base exp) var)))
  (set! the-table nil)
  (put 'deriv '+ sum)
  (put 'deriv '* product)
  (put 'deriv 'expt exponentiation))

; D.

; last three lines of previous function must be changed to
;  (put '+ 'deriv sum)
;  (put '* 'deriv product)
;  (put 'expt 'deriv exponentiation))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.74
;;;;;;;;;;;;;;;;;

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

(define file-A nil)

(define (division-A)
  (define internal-file
    '((suzy	riga		10)
      (sam	liepaja		20)
      (tom	madagascar	50)))
  (define get-name car)
  (define get-address cadr)
  (define get-salary caddr)
  (define (get-record file name)
    (cond ((null? file) false)
	  ((equal? name (get-name (car file))) (car file))
	  (else (get-record (cdr file) name))))

  (set! file-A (attach-tag 'A internal-file))
  (put 'get-salary 'A get-salary)
  (put 'get-record 'A get-record))

(define file-B nil)

(define (division-B)
  (define internal-file
    '((60	30	25)
      (tim	fox	frog)
      (york	paris	dresden)))
  (define get-name cadr)
  (define get-address caddr)
  (define get-salary car)
  (define (get-record file name)
    (cond ((null? (get-name file)) false)
	  ((equal? name (car (get-name file))) (map car file))
	  (else (get-record (map cdr file) name))))

  (set! file-B (attach-tag 'B internal-file))
  (put 'get-salary 'B get-salary)
  (put 'get-record 'B get-record))

(division-A)
(division-B)

(define all-records (list file-A file-B))

; A.
(define (get-record file name)
  (let ((tag (type-tag file)))
    (let ((record ((get 'get-record tag) (contents file) name)))
      (and record (attach-tag tag record)))))

; B.
(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

; C.
(define (find-employee-record files name)
  (and (pair? files)
       (or (get-record (car files) name)
	   (find-employee-record (cdr files) name))))

; D.
; new company must tag their files
;  (set! file-C (attach-tag 'C internal-file))

; and must "put" functions for their selectors
;  (put 'get-salary 'C get-salary)
;  (put 'get-record 'C get-record))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.75
;;;;;;;;;;;;;;;;;

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)	  
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.76
;;;;;;;;;;;;;;;;;

; 1. explicit dispath (table rows)
;	a) to add new operation, new generic operation 
;          must be implemented for all existing object types
;	b) to add new type, each of existing generic operations must be extended

; 2. data driven style (table)
;	a) to add new type new package with all operations must be added 
;          and all must be registred within table (more code than 3b)
;	b) to add new operation it must be added in all existing packages 
;	   and also must be registred in table

; 3. message passing style (table columns)
;	a) to add new operation every existing constructor must be extended
;	   e.g. new (eq? op 'new-operation) in every constructor
;	b) to add new type just add new constructor with all existing operations

;   if system had new types added most often, then message passing style
; would be most appropriate, because changes would be localized due to 3b

;   if system had new operations added most often, explicit dispath
; would be most appropriate, because changes would be localized due to 1a

;;;;;;;;;;;;;;;;;
;;; Exercise 2.77
;;;;;;;;;;;;;;;;;

					; z -> '(complex rectangular 3 . 4)

; (magnitude z)				; global magnitude
; ((get 'magnitude (type-tag z)) z)	; z -> '(rectangular 3 . 4)
; (magnitude z)				; complex package magnitude
; ((get 'magnitude (type-tag z)) z)	; z -> '(3 . 4)
; (magnitude z)				; rectangular package magnitude
; (sqrt (+ (square 3) (square 4)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.78
;;;;;;;;;;;;;;;;;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (attach-tag type-tag contents)
  (if (not (equal? type-tag 'scheme-number))
      (cons type-tag contents)
      contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
	((number? datum) 'scheme-number)
	(error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (cond ((number? datum) datum)
	((pair? datum) (cdr datum))
	(error "Bad tagged datum -- CONTENTS" datum)))

(define (install-scheme-number-package)
  (define (tag x) x)
  (define (convert x) (lower (make-real x)))
  (put 'add '(scheme-number scheme-number) +)
  (put 'sub '(scheme-number scheme-number) -)
  (put 'neg '(scheme-number) -)
  (put 'mul '(scheme-number scheme-number) *)
  (put 'div '(scheme-number scheme-number) /)
  (put 'greatest-common-divisor '(scheme-number scheme-number) gcd)
  (put 'rem '(scheme-number scheme-number) remainder)
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) (lambda (x) (= 0 x)))
  (put 'exp '(scheme-number scheme-number) expt)
  (put 'reduce '(scheme-number scheme-number) list)
  (put 'raise '(scheme-number) convert)
  (put 'project '(scheme-number) convert)
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.79
;;;;;;;;;;;;;;;;;

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
      (cons (* (signum d) (/ n g)) (/ (abs d) g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (= (/ (numer x) (denom x))
	    (/ (numer y) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (= 0 (numer x))))
  (put 'raise '(rational) ; coerce to float
       (lambda (x) (make-real (* 1.0 (/ (numer x) (denom x))))))
  (put 'project '(rational)
       (lambda (x) (make-integer (floor (/ (numer x) (denom x))))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'square-root '(complex) 
       (lambda (x) (error "complex square-root not implemented")))
  (put 'sine '(complex) 
       (lambda (x) (error "complex sine not implemented")))
  (put 'cosine '(complex) 
       (lambda (x) (error "complex cosine not implemented")))
  (put 'arctan '(complex complex) 
       (lambda (x y) (error "complex arctan not implemented")))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (imag-part x) (imag-part y))
			  (= (real-part x) (real-part y)))))

  (put '=zero? '(complex)
       (lambda (x) (and (= 0 (imag-part x))
			(= 0 (real-part x)))))
  (put 'raise '(complex) (lambda (x) #f))
  (put 'project '(complex) (lambda (x) (make-real (real-part x))))
  'done)

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (equ? x y)
  (apply-generic 'equ? x y))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.80
;;;;;;;;;;;;;;;;;

(define (=zero? x)
  (apply-generic '=zero? x))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.81
;;;;;;;;;;;;;;;;;

(define (put-coercion type1 type2 fn)
  (put 'coercion (list type1 type2) fn))

(define (get-coercion type1 type2)
  (get 'coercion (list type1 type2)))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))

; A. Infinite loop will happen,
;    because if apply-generic will not find '(complex complex) in table
;    it will corece it to '(complex complex) and call apply-generic again

; B. Yes! For example if operation is defined only over complex numbers, then
;    it would make sense to coerce '(scheme-number scheme-number) to
;    '(complex complex) to perform that operation

; C.
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((eq? type1 type2)
			 (error "coercion on identical types"))
			(t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.82
;;;;;;;;;;;;;;;;;

(define (super-add x y z) (apply-generic 'super-add x y z))

(define (make-super-add type)
  (put 'super-add (list type type type)
       (lambda (x y z)
	 (define (tag n) (attach-tag type n))
	 (add (tag x) (add (tag y) (tag z))))))

(make-super-add 'integer)
(make-super-add 'rational)
(make-super-add 'real)
(make-super-add 'complex)

(put 'super-add '(scheme-number scheme-number scheme-number) +)

(define (safe-call fn . args)
  (if (procedure? fn) (apply fn args) false))

(define (same-items list)
  (cond ((null? list) true)
	((null? (cdr list)) true)
	((not (eq? (car list) (cadr list))) false)
	(else (same-items (cdr list)))))

(define (apply-generic op . args)
  (define (coercer-of type)
    (lambda (item) (safe-call (get-coercion (type-tag item) type) item)))
  (define (coerce type)
    (let ((coerced-args (map (coercer-of type) args)))
      (if (memq false coerced-args) false coerced-args)))
  (define (try-all-coercions type-tags)
    (if (null? type-tags)
	(error "coercion has failed")
	(let ((coerced-args (coerce (car type-tags))))
	  (if coerced-args
	      (apply apply-generic (cons op coerced-args))
	      (try-all-coercions (cdr type-tags))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
	  (if (same-items type-tags)
	      (error "coercion on identical types")
	      (try-all-coercions type-tags))))))

; function interpolate(x, y, q) = x * q - x * (1 - q),

; sane type cases are

; '(scheme-number scheme-number scheme-number)
; '(rational rational scheme-number)
; '(complex complex scheme-number)

; '(scheme-number scheme-number rational)
; '(rational rational rational)
; '(complex complex rational)

; apply-generic could handle, but will fail coercion on
; '(complex scheme-number scheme-number)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.83
;;;;;;;;;;;;;;;;;

(define (install-number type tagged)
  (put 'add (list type type) (tagged +))
  (put 'sub (list type type) (tagged -))
  (put 'mul (list type type) (tagged *))
  (put 'equ? (list type type) =)
  (put '=zero? (list type) (lambda (x) (= 0 x)))
  (put 'exp (list type type) (tagged expt)))

(define (install-integer)
  (define (tag x) (attach-tag 'integer x))
  (define (tagged f) (lambda x (tag (apply f x))))
  (install-number 'integer tagged)
  (put 'div '(integer integer)
       (lambda (x y)
	 (if (not (= 0 (remainder x y)))
	     (make-rational x y)
	     (tag (/ x y)))))
  (put 'rem '(integer integer) (lambda (x y) (tag (remainder x y))))
  (put 'greatest-common-divisor '(integer integer) 
       (lambda (x y) (tag (gcd x y))))
  (put 'make 'integer (lambda (x) (tag x)))
  (put 'raise '(integer) (lambda (x) (make-rational x 1)))
  (put 'project '(integer) (lambda (x) #f))
  'done)

(define (make-integer n)
  ((get 'make 'integer) (floor n)))

(install-integer)

(define (install-real)
  (define (tag x) (attach-tag 'real x))
  (define (tagged f) (lambda x (tag (apply f x))))
  (install-number 'real tagged)
  (put 'square-root '(real) (lambda (x) (tag (sqrt x))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'arctan '(real real) (lambda (x y) (tag (atan x y))))
  (put 'div '(real real) (tagged /))
  (put 'make 'real (lambda (x) (tag x)))
  (put 'raise '(real) (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'project '(real) (lambda (x) (make-rational (floor (* x 10e12)) 10e12)))
  'done)

(define (make-real n)
  ((get 'make 'real) n))

(install-real)

; integer -> rational -> real -> complex
(define (raise x) (apply-generic 'raise x))

(assert '(raise (make-rational 1 2)) (make-real 0.5))
(assert '(raise (make-real 0.5)) (make-complex-from-real-imag 0.5 0))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.84
;;;;;;;;;;;;;;;;;

(define (equal-types a b)
  (eq? (type-tag a) (type-tag b)))

(define (raise-to a b)
  (cond ((or (not a) (equal-types a b)) a)
	(else (raise-to (raise a) b))))

(define (raise-numbers list)
  ; raise scheme-numbers (this was added for ex 2.86)
  (map (lambda (x) (if (number? x) (raise x) x)) list))
  
(define (is-polynomial? item)
  (eq? 'polynomial (type-tag item)))

(define (get-polynomial-variable item)
  ((get 'variable 'polynomial) (contents item)))

(define (has-polynomial list)
  (cond ((null? list) false)
	((is-polynomial? (car list)) (get-polynomial-variable (car list)))
	(else (has-polynomial (cdr list)))))

(define (coerce-to-polynomials list var)
  (if (null? list) 
      list
      (cons ((get 'coerce 'polynomial) (car list) var)
	    (coerce-to-polynomials (cdr list) var))))

(define (apply-generic-raise op . args)
  (define (raise-args-to list elem)
    (map (lambda (x) (or (raise-to x elem) x)) list))
  (define (arg-raise args)
    (define (arg-raise raised rest)
      (if (null? rest)
	  raised
	  (arg-raise (raise-args-to raised (car rest)) (cdr rest))))
    (let ((var (has-polynomial args)))
      (if var
	  (coerce-to-polynomials args var)
	  (arg-raise args args))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
	  (apply apply-generic-raise 
		 (cons op (if (not (same-items type-tags))
			      (arg-raise (raise-numbers args))
			      (map raise args))))))))

(define apply-generic apply-generic-raise)

(assert '(super-add (make-real 2.0)
		    (make-integer 1) 		    
		    (make-rational 1 2))
	(make-real 3.5))

(assert '(mul (make-rational 1 2)
	      (make-real 2.0))
	(make-real 1.0))

(assert '(super-add (make-real 0.4)
		    (make-complex-from-real-imag 2.3 0)
		    (make-rational 3 2))
	(make-complex-from-real-imag 4.2 0))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.85
;;;;;;;;;;;;;;;;;

(define (project x) (apply-generic 'project x))

(assert '(project (make-complex-from-real-imag 1 1))
	(make-real 1))
(assert '(project (project (make-complex-from-real-imag 1 1)))
	(make-rational 1.0 1.0))
(assert '(project (project (project (make-complex-from-real-imag 1 1))))
	(make-integer 1.0))

(assert '(equ? (make-integer 1) (make-complex-from-real-imag 1 0)) true)
(assert '(equ? (make-integer 1) (make-complex-from-real-imag 1 1)) false)

; fuck, seems that scheme has built in drop
(define (lower x)
  (let ((p (project x)))
    (if (and p (equ? x p))
	(lower p)
	x)))

(assert '(lower (make-complex-from-real-imag 5 0))
	(make-integer 5.0))
(assert '(lower (make-complex-from-real-imag 5 1))
	(make-complex-from-real-imag 5 1))
(assert '(lower (make-real 1.6)) 
	(make-rational 8.0 5.0))
(assert '(lower (make-real 2.0)) 
	(make-integer 2.0))

(define (is-lowerable? x)
  ; we need also equ? to lower, but I will assume that
  ; if project is implemented then equ? is implemented as well
  (and (pair? x) (get 'project (list (type-tag x)))))

(define (apply-generic op . args)  
  (let ((result (apply apply-generic-raise (cons op args))))
    (if (and (is-lowerable? result)
	     (not (eq? op 'project)) ; we want just project without lowering
	     (not (eq? op 'raise)))  ; infinite loop ensues otherwise
	(lower result)
	result)))

(assert '(project (make-real 5.0))
	(make-rational 5.0 1.0))
(assert '(raise (make-real 5.0))
	(make-complex-from-real-imag 5.0 0))
(assert '(mul (make-real 2.0) (make-rational 1 2))
	(make-integer 1.0))
(assert '(mul (make-complex-from-real-imag 0.1 0) (make-real 4.0))
	(make-rational 2.0 5.0))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.86
;;;;;;;;;;;;;;;;;

(define (cosine x) (apply-generic 'cosine x))
(define (sine x) (apply-generic 'sine x))
(define (arctan x y) (apply-generic 'arctan x y))
(define (square-root x) (apply-generic 'square-root x))

(assert '(sine (make-integer 1)) (make-real (sin 1)))
(assert '(cosine (make-integer 0)) (make-integer 1.0))
(assert '(arctan (make-integer 1) (make-integer 1)) (make-real (atan 1 1)))

(assert '(square-root (make-integer 4)) (make-integer 2.0))
(assert '(square-root (make-real 2)) (make-real (sqrt 2)))

(define (squared x) (mul x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (magnitude z)
    (square-root (add (squared (real-part z))
		      (squared (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z)
	    (real-part z)))
  (define (make-from-mag-ang r a) 
    (cons (* r (cosine a))
	  (* r (sine a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)    
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y) 
    (cons (square-root (add (squared x) (squared y)))
          (arctan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))

  (put 'equ? '(complex complex)
       (lambda (x y) (and (equ? (imag-part x) (imag-part y))
			  (equ? (real-part x) (real-part y)))))

  (put '=zero? '(complex)
       (lambda (x) (and (=zero? (imag-part x)) (=zero? (real-part x)))))

  (put 'raise '(complex) (lambda (x) #f))
  (put 'project '(complex) (lambda (x) (real-part x)))
  (put 'neg '(complex)
       (lambda (x) (tag (sub 0 (tag x)))))
  (put 'reduce '(complex complex) 
       (lambda (x y) (list (lower (tag x)) (lower (tag y)))))
  'done)

(define (install-real)
  (put 'raise '(real) (lambda (x)
			(make-complex-from-real-imag (make-real x)
						     (make-integer 0))))
  'done)

(install-rectangular-package)
(install-polar-package)
(install-complex-package)
(install-real)

(assert 
 '(add (make-complex-from-real-imag (make-real 1.5) (make-real 2.0))
       (make-complex-from-real-imag (make-integer 1) (make-rational 1 2))) 
 (make-complex-from-real-imag (make-rational 5.0 2.0) (make-rational 5.0 2.0)))

(assert 
 '(mul (make-complex-from-real-imag (make-real 2) (make-integer 0))
       (make-complex-from-real-imag (make-real 0.5) (make-real 0.5)))
 (make-complex-from-mag-ang (square-root 2) (arctan 0.5 0.5)))

(assert '(raise (make-real 1.0))
	(make-complex-from-real-imag (make-real 1.0) (make-integer 0)))

(assert '(lower (make-complex-from-real-imag (make-real 2.0) (make-integer 0)))
	(make-integer 2.0))

(assert '(mul (make-complex-from-mag-ang 0.5 (/ pi 2)) 
	      (make-complex-from-mag-ang 0.5 (- (/ pi 2))))
	(make-rational 1.0 4.0))

(assert '(mul (make-complex-from-mag-ang (make-real 2.0) (make-real (/ pi -2)))
	      (make-complex-from-mag-ang (make-real 0.5) (make-real (/ pi 2))))
	(make-integer 1.0))

(assert '(mul (make-complex-from-mag-ang (make-real 0.5) 0.0) (make-integer 2))
	(make-integer 1.0))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.87
;;;;;;;;;;;;;;;;;

(define (install-polynomial-package type)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (adjoin-sparse term term-list)
    (cond ((null? term-list) (list term))
	  ((= (order term) (order (car term-list))) 
	   (error "ERROR: term of such order already exists"))
	  ((> (order term) (order (car term-list)))
	   (cons term term-list))
	  (else (cons (car term-list) (adjoin-sparse term (cdr term-list))))))
  (define (generate-list x count)
    (if (= 0 count) nil (cons x (generate-list x (- count 1)))))
  (define (add-with-padding x list count)
    (cons (coeff x) (append (generate-list 0 (- count 1)) list)))
  (define (adjoin-dense term term-list)
    (let ((current-order (- (length term-list) 1)))
      (cond ((> (order term) current-order)
	     (add-with-padding term term-list (- (order term) current-order)))
	    ((= (order term) current-order)
	     (cons (coeff term) (cdr term-list)))
	    (else (cons (car term-list) (adjoin-dense term (cdr term-list)))))))
  (define (adjoin-term term term-list)
    (cond ((eq? type 'sparse) (adjoin-sparse term term-list))
	  ((eq? type 'dense) (adjoin-dense term term-list))))
  (define (the-empty-termlist) '())
  (define (first-term-sparse term-list) (car term-list))
  (define (first-term-dense term-list) 
    (make-term (- (length term-list) 1) (car term-list)))
  (define (first-term term-list)
    (cond ((eq? type 'sparse) (first-term-sparse term-list))
	  ((eq? type 'dense) (first-term-dense term-list))))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list coeff 'x '^ order))
  (define (order term) (cadddr term))
  (define (coeff term) (car term))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (map-terms fn term-list)
    (define (fix-next-term terms fixed-terms)
      (if (empty-termlist? terms)
	  fixed-terms
	  (fix-next-term (rest-terms terms)
			 (adjoin-term (fn (first-term terms))
				      fixed-terms))))
    (fix-next-term term-list (the-empty-termlist)))
  (define (neg-terms term-list)
    (map-terms neg-term term-list))
  (define (number-to-term x)
    (adjoin-term (make-term 0 x) (the-empty-termlist)))

  (define (all-zeros? term-list)
    (cond ((empty-termlist? term-list) true)
	  ((not (=zero? (coeff (first-term term-list)))) false)
	  (else (all-zeros? (rest-terms term-list)))))
      
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (substract (tag p1) (tag p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'variable 'polynomial variable)
  (put '=zero? '(polynomial) 
       (lambda (x) (or (empty-termlist? (term-list x))
		       (all-zeros? (term-list x)))))
  (put 'neg '(polynomial) 
       (lambda (x) (tag (make-poly (variable x) (neg-terms (term-list x))))))
  (put 'coerce 'polynomial
       (lambda (item var)
	 (if (not (is-polynomial? item))
	     (tag (make-poly var (number-to-term item)))
	     item)))
  'done)

(install-polynomial-package 'sparse)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

(assert '(=zero? (make-polynomial 'x '())) true)
(assert '(=zero? (make-polynomial 'x '((0 x ^ 0)))) true)
(assert '(=zero? (make-polynomial 'x '((0 x ^ 4)))) true)
(assert '(=zero? (make-polynomial 'x '((0 x ^ 3) (0 x ^ 0)))) true)
(assert '(=zero? (make-polynomial 'x '((2 x ^ 0) (0 x ^ 0)))) false)
(assert '(=zero? (make-polynomial 'x '((5 x ^ 5) (0 x ^ 0)))) false)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.88
;;;;;;;;;;;;;;;;;

(define (neg x) (apply-generic 'neg x))

(define (substract a b) (add a (neg b)))

(assert '(neg '(polynomial x (5 x ^ 5) (-3 x ^ 2) (4 x ^ 0)))
	(make-polynomial 'x '((-5 x ^ 5) (3 x ^ 2) (-4 x ^ 0))))

(assert '(neg '(polynomial x (6 x ^ 4)))
	(make-polynomial 'x '((-6 x ^ 4))))

(define some-poly '(polynomial x (5 x ^ 5) (-3 x ^ 2) (4 x ^ 0)))
(define misc-poly '(polynomial x (3 x ^ 4) (-2 x ^ 1)))

(assert '(=zero? (sub some-poly some-poly)) true)

(assert '(sub some-poly misc-poly) 
	'(polynomial x (5 x ^ 5) (-3 x ^ 4) (-3 x ^ 2) (2 x ^ 1) (4 x ^ 0)))

(assert '(sub 5 '(polynomial x (-2 x ^ 0)))
	'(polynomial x ((integer . 7.0) x ^ 0)))

(assert '(sub '(polynomial x (-2 x ^ 0)) (make-real 0.5))
	'(polynomial x ((rational -5.0 . 2.0) x ^ 0)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.89
;;;;;;;;;;;;;;;;;

(install-polynomial-package 'dense)

(assert '(sub '(polynomial x 1 2 3 4 5) '(polynomial x 5 4 3 2 1))
	(make-polynomial 'x '(-4 -2 0 2 4)))

(define dense-poly '(polynomial y 2 1))

(assert '(add dense-poly 5) '(polynomial y 2 (integer . 6.0)))

(assert '(add '(polynomial x 1 2 3) (make-polynomial 'x (list 5 dense-poly 1)))
	'(polynomial x 6 (polynomial y 2 (integer . 3.0)) 4))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.90
;;;;;;;;;;;;;;;;;

(define (install-polynomial-package)
  (define (tag p) (attach-tag 'polynomial p))
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))

  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(attach-tag 
	 (type-tag term-list)
	 ((get 'adjoin-term (type-tag term-list)) term (contents term-list)))))
  (define (the-empty-termlist . example) 
    (attach-tag (if (null? example) 'sparse (type-tag (car example))) '()))
  (define (first-term term-list)
    ((get 'first-term (type-tag term-list)) (contents term-list)))
  (define (rest-terms term-list) 
    (attach-tag (type-tag term-list) (cdr (contents term-list))))
  (define (empty-termlist? term-list) (null? (contents term-list)))

  (define (make-term order coeff) (list coeff 'x '^ order))
  (define (order term) (cadddr term))
  (define (coeff term) (car term))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1)) (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	L1
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	L
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))

  (define (var>? a b)
    (string>? 
     (symbol->string a)
     (symbol->string b)))
  
  (define (raise-poly var poly)
    (make-poly var (adjoin-term  
		    (make-term 0 (tag poly))
		    (the-empty-termlist))))

  (define (raise-op fn p1 p2)
    (if (var>? (variable p1) (variable p2))
	(fn p1 (raise-poly (variable p1) p2))
	(fn (raise-poly (variable p2) p1) p2)))

  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(raise-op add-poly p1 p2)))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(raise-op mul-poly p1 p2)))

  (define (neg-term term)
    (make-term (order term) (neg (coeff term))))
  (define (map-terms fn term-list)
    (define (fix-next-term terms fixed-terms)
      (if (empty-termlist? terms)
	  fixed-terms
	  (fix-next-term (rest-terms terms)
			 (adjoin-term (fn (first-term terms))
				      fixed-terms))))
    (fix-next-term term-list (the-empty-termlist term-list)))
  (define (neg-terms term-list)
    (map-terms neg-term term-list))
  (define (number-to-term x)
    (adjoin-term (make-term 0 x) (the-empty-termlist)))
  (define (quotient x) (car x))
  (define (remainder x) (cadr x))
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
	(list L1 L1)
	(let ((t1 (first-term L1))
	      (t2 (first-term L2)))
	  (if (> (order t2) (order t1))
	      (list (the-empty-termlist L1) L1)
	      (let ((new-c (div (coeff t1) (coeff t2)))
		    (new-o (- (order t1) (order t2))))
		(let ((rest-of-result 
		       (div-terms
			(add-terms
			 L1
			 (neg-terms
			  (mul-terms 
			   (adjoin-term 
			    (make-term new-o new-c)
			    (the-empty-termlist L1))
			   L2)))
			L2)))
		  (list (adjoin-term
			 (make-term new-o new-c)
			 (quotient rest-of-result))
			(remainder rest-of-result))))))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(map (lambda (x) (make-poly (variable p1) x))
	     (div-terms (term-list p1) (term-list p2)))
	(error "Polys not in same var -- DIV-POLY" (list p1 p2))))
    
  (define (is-poly-zero? x)
    (or (empty-termlist? (term-list x))
	(all-zeros? (term-list x))))

  (define (apply-to-first-term fn p)
    (if (empty-termlist? (term-list p))
	0
	(fn (first-term (term-list p)))))

  (define (poly-order p)
    (apply-to-first-term order p))

  (define (leading-coeff p)
    (apply-to-first-term coeff p))

  (define (integerizing-factor p1 p2)
    (pow (leading-coeff p2)
	 (sub (add 1 (poly-order p1))
	      (poly-order p2))))
  
  (define (integratize a b)
    (contents (mul (tag a) (integerizing-factor a b))))

  (define (gcd-of-terms terms)
    (cond ((empty-termlist? terms) 1)
	  ((empty-termlist? (rest-terms terms)) (coeff (first-term terms)))
	  (else (greatest-common-divisor 
		 (coeff (first-term terms))
		 (gcd-of-terms (rest-terms terms))))))

  (define (reduce-terms poly)
    (contents (div (tag poly) (gcd-of-terms (term-list poly)))))

  (define (gcd-poly a b)
    (if (is-poly-zero? b)
	a
	(reduce-terms (gcd-poly b (cadr (div-poly (integratize a b) b))))))

  (define (all-zeros? term-list)
    (cond ((empty-termlist? term-list) true)
	  ((not (=zero? (coeff (first-term term-list)))) false)
	  (else (all-zeros? (rest-terms term-list)))))
      
  ;; interface to rest of the system
  (define (install-sparse-polynomial)
    (define (adjoin-sparse term term-list)
      (cond ((null? term-list) (list term))
	    ((= (order term) (order (car term-list))) 
	     (error "ERROR: term of such order already exists"))
	    ((> (order term) (order (car term-list)))
	     (cons term term-list))
	    (else (cons (car term-list) (adjoin-sparse term (cdr term-list))))))
    (define (first-term-sparse term-list) (car term-list))
    (put 'adjoin-term 'sparse adjoin-sparse)
    (put 'first-term 'sparse first-term-sparse))

  (define (install-dense-polynomial)
    (define (generate-list x count)
      (if (= 0 count) nil (cons x (generate-list x (- count 1)))))
    (define (add-with-padding x list count)
      (cons (coeff x) (append (generate-list 0 (- count 1)) list)))
    (define (adjoin-dense term term-list)
      (let ((current-order (- (length term-list) 1)))
	(cond
	 ((> (order term) current-order)
	  (add-with-padding term term-list (- (order term) current-order)))
	 ((= (order term) current-order)
	  (cons (coeff term) (cdr term-list)))
	 (else (cons (car term-list) (adjoin-dense term (cdr term-list)))))))
    (define (first-term-dense term-list) 
      (make-term (- (length term-list) 1) (car term-list)))
    (put 'adjoin-term 'dense adjoin-dense)
    (put 'first-term 'dense first-term-dense))

  (install-sparse-polynomial)
  (install-dense-polynomial)

  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'sub '(polynomial polynomial) 
       (lambda (p1 p2) (substract (tag p1) (tag p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial polynomial) 
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put 'greatest-common-divisor '(polynomial polynomial) 
       (lambda (p1 p2) 
	 (tag (if (> (poly-order p1)
		     (poly-order p2))
		  (gcd-poly p1 p2)
		  (gcd-poly p2 p1)))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'rem '(polynomial polynomial) 
       (lambda (p1 p2) (tag (cadr (div-poly p1 p2)))))
  (put 'div-poly 'polynomial div-poly)

  (put 'variable 'polynomial variable)
  (put '=zero? '(polynomial) is-poly-zero?)
  (put 'neg '(polynomial) 
       (lambda (x) (tag (make-poly (variable x) (neg-terms (term-list x))))))
  (put 'coerce 'polynomial
       (lambda (item var)
	 (if (not (is-polynomial? item))
	     (tag (make-poly var (number-to-term item)))
	     item)))
  (put 'reduce '(polynomial polynomial)
       (lambda (x y)
	 (let ((z (greatest-common-divisor
		   (gcd-of-terms (term-list x))
		   (gcd-of-terms (term-list y)))))
	   (list (div (tag x) z) (div (tag y) z)))))
  'done)

(install-polynomial-package)

(assert '(neg '(polynomial x dense 1 2 3))
	'(polynomial x dense -1 -2 -3))

(assert '(neg '(polynomial x sparse (1 x ^ 2) (2 x ^ 1) (3 x ^ 0)))
	'(polynomial x sparse (-1 x ^ 2) (-2 x ^ 1) (-3 x ^ 0)))

(assert '(add '(polynomial x dense 1 2 3)
	      '(polynomial x sparse (1 x ^ 2) (2 x ^ 1) (3 x ^ 0)))
	'(polynomial x sparse (2 x ^ 2) (4 x ^ 1) (6 x ^ 0)))

(assert '(add '(polynomial x sparse (1 x ^ 2) (2 x ^ 1) (3 x ^ 0))
	      '(polynomial x dense 1 2 3))
	'(polynomial x dense 2 4 6))

(assert '(add '(polynomial x dense 2) 5)
	'(polynomial x sparse ((integer . 7.0) x ^ 0)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.91
;;;;;;;;;;;;;;;;;

(assert '(div '(polynomial x sparse (1 x ^ 5) (-1 x ^ 0))
	      '(polynomial x sparse (1 x ^ 2) (-1 x ^ 0)))
	'((polynomial x sparse (1 x ^ 3) (1 x ^ 1))
	  (polynomial x sparse (1 x ^ 1) (-1 x ^ 0))))

(assert '(div '(polynomial x sparse (1 x ^ 5) (-1 x ^ 0))
	      '(polynomial x dense 1 0 -1))
	'((polynomial x dense 1 0 1 0)
	  (polynomial x dense 1 -1)))

(assert '(div '(polynomial x sparse (1 x ^ 10) (1 x ^ 9) (1 x ^ 8))
	      '(polynomial x sparse (1 x ^ 4)))
	'((polynomial x sparse (1 x ^ 6) (1 x ^ 5) (1 x ^ 4))
	  (polynomial x sparse)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.92
;;;;;;;;;;;;;;;;;

(assert '(add '(polynomial x sparse (1 x ^ 1) (1 x ^ 0))
	      '(polynomial y sparse (1 x ^ 2) (3 x ^ 0)))
	(add '(polynomial y sparse (1 x ^ 2) (3 x ^ 0))
	     '(polynomial x sparse (1 x ^ 1) (1 x ^ 0))))

(assert '(mul '(polynomial x sparse (1 x ^ 1) (1 x ^ 0))
	      '(polynomial y sparse (1 x ^ 2) (3 x ^ 0)))
	(mul '(polynomial y sparse (1 x ^ 2) (3 x ^ 0))
	     '(polynomial x sparse (1 x ^ 1) (1 x ^ 0))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.93
;;;;;;;;;;;;;;;;;

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cadr x))
  (define (make-rat n d)
    (list n d))
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
		   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y)
	 (and (equ? (numer x) (numer y))
	      (equ? (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (=zero? (numer x))))
  (put 'raise '(rational)
       (lambda (x) 
	 (if (not (or (is-polynomial? (numer x)) (is-polynomial? (denom x))))
	     (make-real (* 1.0 (/ (numer x) (denom x))))
	     false)))
  (put 'project '(rational)
       (lambda (x) 
	 (if (not (or (is-polynomial? (numer x)) (is-polynomial? (denom x))))
	     (make-integer (floor (/ (numer x) (denom x))))
	     false)))
  'done)

(install-rational-package)

(define (test-2.93)
  (define p1 (make-polynomial 'x '(sparse (1 x ^ 2)(1 x ^ 0))))
  (define p2 (make-polynomial 'x '(sparse (1 x ^ 3)(1 x ^ 0))))
  (define rf (make-rational p2 p1))
  (assert 
   (list 'add (list 'quote rf) (list 'quote rf)) 
   ; common lisp FTW, same could be written as `(add ,rf ,rf)
   '(rational (polynomial x sparse (2 x ^ 5) (2 x ^ 3) (2 x ^ 2) (2 x ^ 0))
	      (polynomial x sparse (1 x ^ 4) (2 x ^ 2) (1 x ^ 0)))))

(test-2.93)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.94
;;;;;;;;;;;;;;;;;

(define (rem x y)
  (apply-generic 'rem x y))

(put 'div '(polynomial polynomial) 
     (lambda (p1 p2) 
       (attach-tag 'polynomial (car ((get 'div-poly 'polynomial) p1 p2)))))
     
(define (greatest-common-divisor a b)
  (if (=zero? b)
      a
      (greatest-common-divisor b (rem a b))))

(put 'make 'rational
     (lambda (n d)
       (let ((g (greatest-common-divisor n d)))	 
	 (attach-tag 'rational (list (div n g) (div d g))))))

(define (test-2.94)
  (define p1 '(polynomial x sparse (1 x ^ 4) (-1 x ^ 3) (-2 x ^ 2) (2 x ^ 1)))
  (define p2 '(polynomial x sparse (1 x ^ 3) (-1 x ^ 1)))
  (assert
   (list 'greatest-common-divisor (list 'quote p1) (list 'quote p2))
   '(polynomial x sparse (-1 x ^ 2) (1 x ^ 1)))
  (assert
   (list 'make-rational (list 'quote p1) (list 'quote p2))
   '(rational (polynomial x sparse (-1 x ^ 2) (2 x ^ 0)) 
	      (polynomial x sparse (-1 x ^ 1) (-1 x ^ 0)))))

(test-2.94)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.95
;;;;;;;;;;;;;;;;;

(define (test-2.95 value)
  (define p1 '(polynomial x sparse (1 x ^ 2) (-2 x ^ 1) (1 x ^ 0)))
  (define p2 '(polynomial x sparse (11 x ^ 2) (7 x ^ 0)))
  (define p3 '(polynomial x sparse (13 x ^ 1) (5 x ^ 0)))
  (define q1 (mul p1 p2))
  (define q2 (mul p1 p3))
  (assert
   (list 'greatest-common-divisor (list 'quote q1) (list 'quote q2))
   value))

(test-2.95
 '(polynomial x sparse (1458/169 x ^ 2) (-2916/169 x ^ 1) (1458/169 x ^ 0)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.96
;;;;;;;;;;;;;;;;;

(define (pow b p)
  (if (=zero? p)
      1
      (mul b (pow b (sub p 1)))))

(define (greatest-common-divisor a b)
  (apply-generic 'greatest-common-divisor a b))

(test-2.95
 '(polynomial x sparse
	      ((integer .  1.0) x ^ 2)
	      ((integer . -2.0) x ^ 1)
	      ((integer .  1.0) x ^ 0)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.97
;;;;;;;;;;;;;;;;;

(define (reduce a b)
  (apply-generic 'reduce a b))

(put 'make 'rational
     (lambda (n d)
       (let ((g (greatest-common-divisor n d)))	 
	 (attach-tag 'rational (reduce (div n g) (div d g))))))

(assert '(make-rational '(polynomial x sparse (3 x ^ 2) (3 x ^ 0))
			'(polynomial x sparse (2 x ^ 1)))
	'(rational (polynomial x sparse 
			       ((integer . 3.0) x ^ 2) 
			       ((integer . 3.0) x ^ 0))
		   (polynomial x sparse 
			       ((integer . 2.0) x ^ 1))))

(assert '(make-rational '(polynomial x sparse (4 x ^ 2) (6 x ^ 0))
			'(polynomial x sparse (2 x ^ 1)))
	'(rational (polynomial x sparse 
			       ((integer . 2.0) x ^ 2) 
			       ((integer . 3.0) x ^ 0))
		   (polynomial x sparse 
			       ((integer . 1.0) x ^ 1))))

(define (test-2.97)
  (define p1 (make-polynomial 'x '(sparse (1 x ^ 1) (1 x ^ 0))))
  (define p2 (make-polynomial 'x '(sparse (1 x ^ 3) (-1 x ^ 0))))
  (define p3 (make-polynomial 'x '(sparse (1 x ^ 1))))
  (define p4 (make-polynomial 'x '(sparse (1 x ^ 2) (-1 x ^ 0))))

  (define rf1 (make-rational p1 p2))
  (define rf2 (make-rational p3 p4))

  ; also checked by hand
  (assert
   (list 'add (list 'quote rf1) (list 'quote rf2))
   '(rational
    (polynomial x sparse
		((integer . 1.0) x ^ 4)
		((integer . 1.0) x ^ 3)
		((integer . 1.0) x ^ 2)
		((integer . -2.0) x ^ 1)
		((integer . -1.0) x ^ 0))
    (polynomial x sparse
		((integer . 1.0) x ^ 5)
		((integer . -1.0) x ^ 3)
		((integer . -1.0) x ^ 2)
		((integer . 1.0) x ^ 0)))))

(test-2.97)