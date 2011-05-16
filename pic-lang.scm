 ; only for MIT scheme

(load "pic-lang/psgo")

;;;;;;;;;;;;;;;;;
;;; Exercise 2.44
;;;;;;;;;;;;;;;;;

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.45
;;;;;;;;;;;;;;;;;

(define (split fn1 fn2)
  (define (local-split painter n)
    (if (= n 0)
	painter
	(let ((smaller (local-split painter (- n 1))))
	  (fn1 painter (fn2 smaller smaller)))))
  local-split)
  
(define right-split-2 (split beside below))
(define up-split-2 (split below beside))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.46
;;;;;;;;;;;;;;;;;

(define make-vect cons)
(define xcor-vect car)
(define ycor-vect cdr)

(define (vect-op fn v1 v2)
  (make-vect
   (fn (xcor-vect v1) (xcor-vect v2))
   (fn (ycor-vect v1) (ycor-vect v2))))

(define (add-vect v1 v2)
  (vect-op + v1 v2))

(define (sub-vect v1 v2)
  (vect-op - v1 v2))

(define (scale-vect v s)
  (vect-op * v (make-vect s s)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.47
;;;;;;;;;;;;;;;;;

; A.
(define (make-frame-a origin edge1 edge2)
  (list origin edge1 edge2))

(define origin-frame-a car)
(define edge1-frame-a cadr)
(define edge2-frame-a caddr)

; B.
(define (make-frame-b origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define origin-frame-b car)
(define edge1-frame-b cadr)
(define edge2-frame-b cddr)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.48
;;;;;;;;;;;;;;;;;

(define make-segment cons)
(define segment-start car)
(define segment-end cdr)

;;;;;;;;;;;;;;;;;
;;; Exercise 2.49
;;;;;;;;;;;;;;;;;

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line-on-screen
        ((frame-coord-map frame) (segment-start segment))
        ((frame-coord-map frame) (segment-end segment))))
     segment-list)))

(define frame-outline-painter
  (segments->painter 
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
	 (make-segment (make-vect 0 1) (make-vect 1 1))
	 (make-segment (make-vect 1 1) (make-vect 1 0))
	 (make-segment (make-vect 1 0) (make-vect 0 0)))))

(define x-painter
  (segments->painter 
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
	 (make-segment (make-vect 1 0) (make-vect 0 1)))))

(define diamond-painter 
  (segments->painter 
   (list (make-segment (make-vect 0.0 0.5) (make-vect 0.5 0.0))
	 (make-segment (make-vect 0.5 0.0) (make-vect 1.0 0.5))
	 (make-segment (make-vect 1.0 0.5) (make-vect 0.5 1.0))
	 (make-segment (make-vect 0.5 1.0) (make-vect 0.0 0.5)))))

; who need fucking wave pattern when you have this beautiful l-system
(load "l-system") ; it took 15 minutes to get abop to generate this

;;;;;;;;;;;;;;;;;
;;; Exercise 2.50
;;;;;;;;;;;;;;;;;

(define (debug . rest) (map display rest) (display ""))

(define (transform-painter-2 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (flip-horiz-2 painter)
  (transform-painter-2 painter
		       (make-vect 1.0 0.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))

(define (rotate-180-2 painter)
  (transform-painter-2 painter
		       (make-vect 1.0 1.0)
		       (make-vect 0.0 1.0)
		       (make-vect 1.0 0.0)))

(define (rotate-270-2 painter)
  (transform-painter-2 painter
		       (make-vect 0.0 1.0)		       
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.51
;;;;;;;;;;;;;;;;;

(define (below-2 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-left
           (transform-painter-2 painter1
				(make-vect 0.0 0.0)
				(make-vect 1.0 0.0)
				split-point))
          (paint-right
           (transform-painter-2 painter2
				split-point
				(make-vect 1.0 0.5)
				(make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (below-3 painter1 painter2)
  (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;;;;;;;;;;;;;;;;;
;;; Exercise 2.52
;;;;;;;;;;;;;;;;;

; A. I don't have a wave pattern so I'll just put my l-system in a box
(define box
  (list (make-segment (make-vect 0.02 0.02) (make-vect 0.02 0.98))
	(make-segment (make-vect 0.02 0.98) (make-vect 0.98 0.98))
	(make-segment (make-vect 0.98 0.98) (make-vect 0.98 0.02))
	(make-segment (make-vect 0.98 0.02) (make-vect 0.02 0.02))))

(define boxed-l-system (segments->painter (append box l-system)))

; B.
(define (corner-split-modified painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((corner (corner-split painter (- n 1))))
          (beside (below painter up)
                  (below right corner))))))

; C.
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
    (combine4 (corner-split (flip-horiz painter) n))))

; frog is originaly looking outward
; exercise 2.52-c modifies square-limit to make it look in-ward
; (paint screen (square-limit frog 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Penrose and Tree of Frogs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pi 3.1415926535897932385)

(define (to-radians angle)
  (* angle (/ pi 180.0)))

(define (to-degrees angle)
  (* (/ angle pi) 180.0))

(define (transform-painter-3 painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))

(define (calc-side angle)
  (let ((squash-angle (to-radians (* 0.5 (- 90 angle)))))
    (let ((side (tan squash-angle)))
      (/ side (+ 1 side)))))
 
(define (squash painter angle)
  (let ((y (calc-side angle)))
    (transform-painter-2 painter
			 (make-vect 0.0 0.0)
			 (make-vect (- 1 y) y)
			 (make-vect y (- 1 y)))))

(define (scale painter q)
  (transform-painter-2 painter
		       (make-vect 0.0 0.0)
		       (make-vect q 0.0)
		       (make-vect 0.0 q)))

(define (move painter vector)
  (transform-painter-2 painter
		       vector
		       (add-vect vector (make-vect 1.0 0.0))
		       (add-vect vector (make-vect 0.0 1.0))))

(define (arb-rotate painter angle)
  (define (arb-rotate-radians angle)
    (let ((angle2 (+ (* 0.5 pi) angle)))
      (transform-painter-2 painter
			   (make-vect 0.0 0.0)		       
			   (make-vect (cos angle) (sin angle))
			   (make-vect (cos angle2) (sin angle2)))))
  (arb-rotate-radians (to-radians angle)))

(define (super-superpose a b . n)
  (if (null? n)
      (superpose a b)
      (superpose a (apply super-superpose (cons b (cons (car n) (cdr n)))))))

(define boxed-frog (superpose frog frame-outline-painter))

(define boxed-fly (superpose fly frame-outline-painter))

(define (branch s) (arb-rotate (scale (squash boxed-frog 10) s) 45))

(define (frog-tree angle i)
  (let ((size (* 0.22 i 0.2)))
    (let ((jump (* size (sqrt 2))))
      (if (= i 0)
	  (branch size)
	  (superpose
	   (branch size)
	   (move (superpose
		  (arb-rotate (frog-tree angle (- i 1)) angle)
		  (arb-rotate (frog-tree angle (- i 1)) (- angle)))
		 (make-vect 0.0 jump)))))))

(define final-frog-tree
  (move (frog-tree 30 5) (make-vect 0.5 0.05)))

; (paint screen final-frog-tree)

(define (calc-rombus-side angle)
  (let ((x (calc-side angle)))
    (sqrt (+ (expt x 2) (expt (- 1.0 x) 2)))))

(define rombus-72-scale 0.1) ; change this for differt tile sizes

(define rombus-side (* rombus-72-scale (calc-rombus-side 72)))

(define rombus-36-scale (/ rombus-side (calc-rombus-side 36)))

(define rombus-36
  (arb-rotate (scale (squash boxed-fly 36) rombus-36-scale) 45))

(define rombus-72
  (arb-rotate (scale (squash boxed-frog 72) rombus-72-scale) 45))

(define (dec i) (- i 1))

(define (scale-factor i)
  (expt golden-ratio (- i 2)))

(define (move-rombus-side i painter)
  (move painter (make-vect 0.0 (* (scale-factor i) rombus-side))))

(define (move-rombus-length i painter)
  (move painter (make-vect 0.0 (* (scale-factor i) (sqrt 2) rombus-72-scale))))

(define (rule-2 i s)
  (if (= i 1)
      rombus-36
      (arb-rotate
       (move-rombus-side (+ i 1)
	(arb-rotate
	 (super-superpose
	  (arb-rotate (rule-1 (dec i)) 108)					
	  (arb-rotate (rule-1 (dec i)) -108)
	  (arb-rotate (rule-2 (dec i) -1) -162)
	  (arb-rotate (rule-2 (dec i) 1) 162))
	(* s 72)))
       (* s 18))))

(define golden-ratio (* 0.5 (+ 1 (sqrt 5))))

(define (flip i rombus)
  (move-rombus-length i (arb-rotate rombus 180)))

(define (rule-1 i)
  (if (= i 1)
      rombus-72
      (move-rombus-length i
       (super-superpose
	(move-rombus-side i
	 (superpose
	  (arb-rotate (rule-1 (dec i)) 144)
	  (arb-rotate (rule-1 (dec i)) -144)))
	(arb-rotate (rule-1 (dec i)) 180)
	(arb-rotate (rule-2 (dec i) -1) 126)
	(arb-rotate (rule-2 (dec i) 1) -126)))))

(define (penrose i)
  (define (initial-star n)
    (if (= n 1)
	(arb-rotate (rule-1 i) 72)
	(arb-rotate (superpose (rule-1 i) (initial-star (- n 1))) 72)))
  (initial-star 5))

(define (final-penrose i)
  (move (penrose i) (make-vect 0.5 0.5)))

(paint screen (final-penrose 5))

