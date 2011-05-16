(load "evaluator")

;;;;;;;;;;;;;;;;;
;;; Exercise 4.1
;;;;;;;;;;;;;;;;;

;; guaranteed right to left
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((x (list-of-values (rest-operands exps) env)))
	(cons (eval (first-operand exps) env)
	      x))))

;; guaranteed left to right
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((x (eval (first-operand exps) env)))
	(cons x
	      (list-of-values (rest-operands exps) env)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.2
;;;;;;;;;;;;;;;;;

; A. 
; in Louis's eval (define x 3) will pass "application?" test
; and then it will look for "define" procedure in environment

; B. 
; (define (application? exp) (tagged-list? exp 'call))
; (define (operator exp) (cadr exp))
; (define (operands exp) (cddr exp))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.3
;;;;;;;;;;;;;;;;;

(define (eval-self exp env) exp)

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))

(define (eval-application exp env)
  (weak-apply (eval (operator exp) env) (list-of-values (operands exp) env)))

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp) (lambda-body exp) env))

(define (eval-quoted exp env)
  (text-of-quotation exp))

(define (eval-cond exp env)
  (eval (cond->if exp) env))

(define (add-eval-rule tester eval-fn)
  (set! eval-rules (cons (list tester eval-fn) eval-rules)))

(define (eval-variable exp env)
  ; this is needed because we want to redefine lookup-variable-value later
  (lookup-variable-value exp env))

(define eval-rules '())

(define (add-basic-rules)
  (add-eval-rule application?	eval-application)
  (add-eval-rule cond?		eval-cond)
  (add-eval-rule begin?		eval-begin)
  (add-eval-rule lambda?	eval-lambda)
  (add-eval-rule if?		eval-if)
  (add-eval-rule definition?	eval-definition)
  (add-eval-rule assignment?	eval-assignment)
  (add-eval-rule quoted?	eval-quoted)
  (add-eval-rule variable?	eval-variable)
  (add-eval-rule self-evaluating? eval-self))

(add-basic-rules)

(define (eval exp env)
  (define (try-rules rules)
    (cond ((null? rules) (error "Unknown expression type -- EVAL" exp))
	  (((caar rules) exp) ((cadar rules) exp env))
	  (else (try-rules (cdr rules)))))
  (try-rules eval-rules))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.4
;;;;;;;;;;;;;;;;;

(define (false? x)
  (or (eq? x false)))

(define (true? x)
  (not (false? x)))

;;; and
(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and exp env)
  (define (test exps last)
    (if (or (null? exps) (false? last))
	last
	(test (cdr exps) (eval (car exps) env))))
  (test (cdr exp) true))

(add-eval-rule and? eval-and)

(define (test-and)
  (assert '(eval '(and ) the-global-environment) true)
  (assert '(eval '(and 'doh) the-global-environment) 'doh)
  (assert '(eval '(and 1 2 3) the-global-environment) 3)
  (assert '(eval '(and 1 false 3) the-global-environment) false))

(test-and)

;;; or
(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or exp env)
  (define (test exps last)
    (if (or (null? exps) (true? last))
	last
	(test (cdr exps) (eval (car exps) env))))
  (test (cdr exp) false))

(add-eval-rule or? eval-or)

(define (test-or)
  (assert '(eval '(or ) the-global-environment) false)
  (assert '(eval '(or 1 2 3) the-global-environment) 1)
  (assert '(eval '(or false 2) the-global-environment) 2)
  (assert '(eval '(or false false) the-global-environment) false))

(test-or)

;;; and derived expression
(define (and->if exp)
  (cond ((null? exp) 'true)
	((null? (cdr exp)) (car exp))
	(else (make-if (car exp)
		       (and->if (cdr exp))
		       'false))))

(define (eval-deriv-and exp env)
  (eval (and->if (cdr exp)) env))

(add-eval-rule and? eval-deriv-and)

(test-and)

;;; or derived expression
(define (or->if exp)  
  ; this implemenetation does not allow assignment into "or", but it's testable
  ; because I use (or x y z) == (if x x (if y y (if z z false)))
  ; SICP does not provide properly working cond/let/lambda yet
  ; maybe reimplement (or x y z) == (cond (x) (y) (z) (else 'false)) later
  (if (null? exp)
      'false
      (make-if (car exp)
	       (car exp)
	       (or->if (cdr exp)))))

(define (eval-deriv-or exp env)
  (eval (or->if (cdr exp)) env))

(add-eval-rule or? eval-deriv-or)

(test-or)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.5
;;;;;;;;;;;;;;;;;

(define (use-lambda var exp body)
  (list (list 'lambda (list var) body) exp))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
	    (use-lambda 
	     'tmp-cond-var
	     (cond-predicate first)
	     (make-if 'tmp-cond-var
		      (cond ((null? (cond-actions first)) 'tmp-cond-var)
			    ((eq? '=> (car (cond-actions first)))
			     (list (cadr (cond-actions first)) 
				   'tmp-cond-var))
			    (else (sequence->exp (cond-actions first))))
		      (expand-clauses rest)))))))

(assert '(eval '(cond (1)) the-global-environment) 1)
(assert '(eval '(cond (2 => sqrt)) the-global-environment) 1.4142135623730951)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.6
;;;;;;;;;;;;;;;;;

(define (let? exp)
  (tagged-list? exp 'let))

(define (let->combination exp)
  (cons (cons 'lambda 
	      (cons (map car (cadr exp)) 
		    (cddr exp)))
	(map cadr (cadr exp))))

(define (eval-let exp env)
  (eval (let->combination exp) env))
		   
(add-eval-rule let? eval-let)

(assert '(eval '(let ((x 1) (y 2) (z 3)) (+ x y z)) the-global-environment) 6)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.7
;;;;;;;;;;;;;;;;;

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-lets exp)
  (define (construct args)
    (if (null? args)
	(cons 'let (cons '() (cddr exp)))
	(list 'let (list (car args)) (construct (cdr args)))))
  (construct (cadr exp)))

(define (eval-let* exp env)
  (eval (let*->nested-lets exp) env))

(add-eval-rule let*? eval-let*)

(assert '(eval '(let* ((x 3) (y (+ x 2)) (z (+ x y 5))) (* x z))
	       the-global-environment)
	39)

(assert '(eval '(let () 4 5) the-global-environment) 5)
(assert '(eval '(let* () 5 6) the-global-environment) 6)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.8
;;;;;;;;;;;;;;;;;

(define let->combination-old let->combination)

(define (make-fn-def name params body)
  (cons 'define (cons (cons name params) body)))

(define (let->combination-new exp)
  (make-begin
   (list (make-fn-def (cadr exp) (map car (caddr exp)) (cdddr exp))     
	 (cons (cadr exp) (map cadr (caddr exp))))))
  
(define (let->combination exp)
  (if (symbol? (cadr exp))
      (let->combination-new exp)
      (let->combination-old exp)))

(assert '(eval '(let fib-iter ((a 1) (b 0) (count 20))
		  (if (= count 0) b (fib-iter (+ a b) a (- count 1))))
	       the-global-environment)
	6765)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.9
;;;;;;;;;;;;;;;;;

; (dotimes (<var> <count>) <body>)

; (let loop ((<var> 0))
;   <body>
;   (if (< <var> (- <count> 1))
;       (loop (+ 1 <var>))
;       'done))

(define (dotimes? exp)
  (tagged-list? exp 'dotimes))

(define (attach-at-end list x)
  (set-cdr! (last-pair list) x)
  list)

(define (construct-dotimes exp)
  (attach-at-end
   (attach-at-end 
    (list 'let 'loop (list (list (caadr exp) 0)))
    (cddr exp))
   (list (make-if (list '< (caadr exp) (list '- (cadadr exp) 1))
		  (list 'loop (list '+ 1 (caadr exp)))
		  ''done))))

(define (eval-dotimes exp env)
  (eval (construct-dotimes exp) env))

(add-eval-rule dotimes? eval-dotimes)

(assert '(eval '(let ((x 0))
		  (dotimes (i 10) (set! x (+ x i)))
		  x)
	       the-global-environment)
	45)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.10
;;;;;;;;;;;;;;;;;

(define (latvian-scheme)
  (define (make-begin seq)
    (cons 'saakums seq))
  (define (make-if predicate consequent alternative)
    (list 'ja predicate consequent alternative))
  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'savaadaak))
  (define (key-checker sym)
    (lambda (exp) (tagged-list? exp sym)))
  (define (make-fn-def name params body)
    (cons 'defineet (cons (cons name params) body)))

  (set! eval-rules '())

  (add-eval-rule application?			eval-application)
  (add-eval-rule lambda?			eval-lambda)
  (add-eval-rule (key-checker 'saakums)		eval-begin)
  (add-eval-rule (key-checker 'pie-nosaciijuma)	eval-cond)
  (add-eval-rule (key-checker 'ja)		eval-if)
  (add-eval-rule (key-checker 'defineet)	eval-definition)
  (add-eval-rule (key-checker 'uzstaadiit)	eval-assignment)
  (add-eval-rule (key-checker 'citeet)		eval-quoted)
  (add-eval-rule variable?			lookup-variable-value)
  (add-eval-rule self-evaluating?		eval-self)
  (add-eval-rule boolean?		(lambda (exp env) (not (false? exp))))
  (add-eval-rule (key-checker 'dariit-reizes)	eval-dotimes)
  (add-eval-rule (key-checker 'un)		eval-and)
  (add-eval-rule (key-checker 'vai)		eval-or)
  (add-eval-rule (key-checker 'lai)		eval-let)
  (add-eval-rule (key-checker 'lai*)		eval-let)
  
  (assert '(eval '(saakums
		   (defineet (faktoriaalis n)
		     (ja (= n 0)
			 1
			 (* n (faktoriaalis (- n 1)))))
		   (lai ((x 0))
	             (uzstaadiit x (faktoriaalis 7))
		     x))
		 the-global-environment)
	  5040))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.11
;;;;;;;;;;;;;;;;;

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))
(define (frame-pairs frame)
  (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(define (find-variable var env)
  (define (env-loop env)
    (define (scan pairs)
      (cond ((null? pairs)
             (env-loop (enclosing-environment env)))
            ((eq? var (caar pairs))
             (car pairs))
            (else (scan (cdr pairs)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
	(scan (frame-pairs (first-frame env)))))
  (env-loop env))

(define (lookup-variable-value var env)
  (cdr (find-variable var env)))

(define (set-variable-value! var val env)
  (set-cdr! (find-variable var env) val))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan pairs)
      (cond ((null? pairs)
             (add-binding-to-frame! var val frame))
            ((eq? var (caar pairs))
             (set-cdr! (car pairs)  val))
            (else (scan (cdr pairs)))))
    (scan (frame-pairs frame))))

(define the-global-environment (setup-environment))

(define test-environment '((frame)))

(define (test-4.11)
  (eval '(define x 5) test-environment)
  (assert 'test-environment '((frame (x . 5))))
  (eval '(set! x 6) test-environment)
  (assert 'test-environment '((frame (x . 6))))
  (assert '(eval 'x test-environment) 6))

(test-4.11)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.12
;;;;;;;;;;;;;;;;;

(define (find-variable-in-frame var frame)
  (define (scan pairs)
    (cond ((null? pairs) false)
	  ((eq? var (caar pairs)) (car pairs))
	  (else (scan (cdr pairs)))))
  (scan (frame-pairs frame)))
  
(define (find-variable var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
	(or (find-variable-in-frame var (first-frame env))
	    (env-loop (enclosing-environment env)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((pair (find-variable-in-frame var frame)))
      (if pair
	  (set-cdr! pair val)
	  (add-binding-to-frame! var val frame)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.13
;;;;;;;;;;;;;;;;;

; I think that make-unbound! should go in depth,
; because otherwise for example in (let ((x 0)) <body>) 
; it would be only possible to unbind variable x
; or in some procedure it would be possible to
; unbind only procedure parameter variables

(define (remove-variable-form-frame var frame)
  (define (inspect pairs)
    (cond ((null? (cdr pairs)) false)
	  ((eq? var (caadr pairs))
	   (set-cdr! pairs (cddr pairs))
	   true)
	  (else (inspect (cdr pairs)))))
  (inspect frame))

(define (remove-from-environment var env)
  (or (eq? env the-empty-environment)
      (remove-variable-form-frame var (first-frame env))
      (remove-from-environment var (enclosing-environment env))))

(add-eval-rule
 (lambda (exp) (tagged-list? exp 'make-unbound!))
 (lambda (exp env) (remove-from-environment (cadr exp) env)))

(define (test-4.13)
  (eval '(define x 5) test-environment)
  (assert 'test-environment '((frame (x . 5))))
  (eval '(let ((x 0)) (make-unbound! x)) test-environment)
  (assert 'test-environment '((frame (x . 5))))
  (eval '(let ((y 0)) (make-unbound! x)) test-environment)
  (assert 'test-environment '((frame))))

(test-4.13)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.14
;;;;;;;;;;;;;;;;;

; Louis's map does not work because map takes procedure as first argument
; (it high order function) and metacircular evaluator tries to pass
; it procedure object something like (procedure <args> <body> <environment>)
; but native scheme procedure objects is different therefore map fails

; Eva's map works because it is defined as metacircular evaluator structure
; that uses primitive functions cons, car and cdr 
; which do not care about their arguments

;;;;;;;;;;;;;;;;;
;;; Exercise 4.15
;;;;;;;;;;;;;;;;;

;(define (run-forever) (run-forever))

;(define (try p)
;  (if (halts? p p)
;      (run-forever)
;      'halted))

; for (try try) there is two possible cases:

; if (halts? try try) returns true - that (try try) should halt,
; then (try try) calls run-forever and never stops

; if (halts? try try) return false - that (try try) should loop forever,
; then (try try) halts by returning 'halted

; in both cases halts? returns exactly oposite what (try try) actually does
; therefore such function halts? is impossible to construct

;;;;;;;;;;;;;;;;;
;;; Exercise 4.16
;;;;;;;;;;;;;;;;;

; A.
(define (lookup-variable-value var env)
  (let ((val (cdr (find-variable var env))))
    (if (eq? val '*unassigned*)
	(error "LOOKUP-VARIABLE-VALUE *unassigned* in variable" var)
	val)))

; B.
(define (transform-definition exp)
  (if (definition? exp)
      (list 'set! (definition-variable exp) (definition-value exp))
      exp))

(define (scan-out-defines body)
  (define (make-let-variable exp)
    (list (definition-variable exp) ''*unassigned*))
  (let ((defs (filter definition? body)))
    (if (null? defs)
	body ; this is to avoid nasty recursion, because each let is new lambda
	(list
	 (attach-at-end
	  (list 'let (map make-let-variable defs))
	  (map transform-definition body))))))

(assert '(scan-out-defines '((define u <e1>) (define v <e2>) <e3>))
	'((let ((u '*unassigned*) (v '*unassigned*)) 
	    (set! u <e1>) (set! v <e2>) <e3>)))

; C.
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

; I choose to install scan-out-defines in make-procedure because that way this
; transformation will be done once - at procedure specification time

; If scan-out-defines is installed in procedure-body that transformation
; is done at each procedure call when procedure-body is requested
; that is very, very inefficient

;;;;;;;;;;;;;;;;;
;;; Exercise 4.17
;;;;;;;;;;;;;;;;;

;(lambda <vars>
;  (define u <e1>)
;  (define v <e2>)
;  <e3>)

;    +---------------------+
;E1: |enclosing environment|
;    +---------------------+
;              |
;    +---------------------+
;E2: |    <vars>, u, v     | <e3>
;    +---------------------+

;(lambda <vars>
;  (let ((u '*unassigned*)
;        (v '*unassigned*))
;    (set! u <e1>)
;    (set! v <e2>)
;    <e3>))
;
; let is transformed into lambda with arguments u and v

;    +---------------------+
;E1: |enclosing environment|
;    +---------------------+
;              |
;    +---------------------+
;E2: |       <vars>        |
;    +---------------------+
;              |
;    +---------------------+
;E3: |        u, v         | <e3>
;    +---------------------+

; * Explain why this difference in environment structure can never 
; make a difference in the behavior of a correct program.

; Because after scan-out (let ...) is only expresion in body of lambda,
; therefore one envirnment with <vars>, u and v is the same thing as
; two envirnments - one with <vars>, other with u and v

; * Design a way to make the interpreter implement the ``simultaneous''
; scope rule for internal definitions without constructing the extra frame.

;(lambda (<vars> u v)
;    (set! u <e1>)
;    (set! v <e2>)
;    <e3>)

; - in make-procedure append body's define 
;   variables to lambda parameters
; - change all define to set! as in previous exercise
; - store somewhere in compound procedure object
;   that there is extra implicit arguments
; - in apply function append to arguments 
;   proper amount of *unassigned* symbols

; ofcourse there whould be problem with:

; (lambda (x)
;   (define x 'something)
;   ...)

; but it is sick anyways and our evaluator can handle this:

; (eval '((lambda (x x) x) 'thing 'something) the-global-environment)

(define (procedure-implicit-args p) (car (cddddr p)))

(define (make-procedure parameters body env)
  (let ((defs (filter definition? body)))
    (list 'procedure 
	  (append parameters (map definition-variable defs))
	  (map transform-definition body)
	  env 
	  (make-list (length defs) '*unassigned*))))

(define (weak-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (append arguments (procedure-implicit-args procedure)) ; ***
             (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- WEAK-APPLY" procedure))))

(define (without-environment object)
  (list 'compound-procedure
	(procedure-parameters object)
	(procedure-body object)
	'<procedure-env>
	(procedure-implicit-args object)))

(assert '(without-environment
	  (eval '(begin 
		   (define (f1 a)
		     (define (f2 b) b)
		     (f2 a))
		   f1)
		the-global-environment))
	'(compound-procedure (a f2)
			     ((set! f2 (lambda (b) b)) (f2 a))
			     <procedure-env>
			     (*unassigned*)))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.18
;;;;;;;;;;;;;;;;;

; *** does not work:
;(define (solve f y0 dt)
;  (let ((y nil)
;	(dy nil))
;    (let ((a (integral (delay dy) y0 dt))
; 	  (b (stream-map f y)))
;      (set! y a)
;      (set! dy b)
;      y)))

; above code does not work because when evaluator evaluates (stream-map f y),
; stream-map is ordinary compound procedure and arguments to it is evaluated
; before they are passed to stream-map, and y at that time is nil
; assignment of a to y is done one line later (lower)

; *** does work:
;(define (solve f y0 dt)
;  (let ((y nil)
;	(dy nil))
;    (set! y (integral (delay dy) y0 dt))
;    (set! dy (stream-map f y))
;    y))

; no problem here, at time of evaluation of (stream-map f y) 
; (integral (delay dy) y0 dt) is already assigned to y

;;;;;;;;;;;;;;;;;
;;; Exercise 4.19
;;;;;;;;;;;;;;;;;

; Which (if any) of these viewpoints do you support?

; after reading footnote (26) it is impossible not to be biased:

; (26) The MIT implementors of Scheme support Alyssa on the following grounds: Eva is in principle correct -- the definitions should be regarded as simultaneous. But it seems difficult to implement a general, efficient mechanism that does what Eva requires. In the absence of such a mechanism, it is better to generate an error in the difficult cases of simultaneous definitions (Alyssa's notion) than to produce an incorrect answer (as Ben would have it).

; Can you devise a way to implement internal definitions
; so that they behave as Eva prefers?

; It is not impossible to change order of definitions,
; but as footnote (26) says it is difficult and I don't see the point.

;;;;;;;;;;;;;;;;;
;;; Exercise 4.20
;;;;;;;;;;;;;;;;;

; A.
(define (letrec->let exp)
  (attach-at-end
   (attach-at-end
    (list 'let (map (lambda (x) (list (car x) ''*unassigned*)) (car exp)))
    (map (lambda (x) (list 'set! (car x) (cadr x))) (car exp)))
   (cdr exp)))
  
(define (letrec? exp)
  (tagged-list? exp 'letrec))

(add-eval-rule letrec? (lambda (exp env) (eval (letrec->let (cdr exp)) env)))

(assert '(eval '(letrec ((fak (lambda (n) (if (= n 1) 1 (* n (fak (- n 1)))))))
		  (fak 10))
	       the-global-environment)
	3628800)

; B.
; see ex-4.20-let.png
; see ex-4.20-letrec.png

;;;;;;;;;;;;;;;;;
;;; Exercise 4.21
;;;;;;;;;;;;;;;;;

; A. factorial worked, here is fibonacci:
(assert '((lambda (n)
	    ((lambda (fib)
	       (fib fib n))
	     (lambda (ft k)
	       (cond ((= k 0) 0)
		     ((= k 1) 1)
		     (else (+ (ft ft (- k 1))
			      (ft ft (- k 2))))))))
	  20)
	6765)

; B.
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.22
;;;;;;;;;;;;;;;;;

(load "analyze")

(define (analyze-let exp)
  (let ((let-vars (map car (cadr exp)))
	(let-forms (map cadr (cadr exp)))
	(let-body (cddr exp)))
    (analyze (cons (make-lambda let-vars let-body) let-forms))))

(define the-global-environment (setup-environment))

(assert '(analyzing-eval '(let ((a 7) (b 6)) (* a b))
			 the-global-environment)
	42)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.23
;;;;;;;;;;;;;;;;;

; by evaluating:
; (lambda () 5)

; text procedure would yield:
; (lambda (env) 5)

; while Alyssa's procedure would yield:
; (lambda (env) (execute-sequence '((lambda (env) 5)) env)

; with execute-sequence bound to procedure:
; (define (execute-sequence procs env)
;   (cond ((null? (cdr procs)) ((car procs) env))
;         (else ((car procs) env)
;               (execute-sequence (cdr procs) env))))

; by evaluating:
; (lambda () 5 6)

; text procedure would yield:
; (lambda (env)
;   ((lambda (env) 5) env)
;   ((lambda (env) 6) env))

; while Alyssa's procedure would yield:
; (lambda (env) (execute-sequence '((lambda (env) 5) (lambda (env) 6)) env)

; text procedure for n expresion sequence would produce exectuion
; procedrure that would make n lambda function calls 

; Alyssa's procedure for n expresion sequence would produce exectuion
; procedrure that would make n recursive execute-sequence function calls,
; with n tests for null?, 2*n cdr calls, n car calls

;;;;;;;;;;;;;;;;;
;;; Exercise 4.24
;;;;;;;;;;;;;;;;;

(define (time fn)
  (let ((start (runtime)))
    (let ((ret (fn)))
      (debug "time = " (- (runtime) start) "\n")
      ret)))      

(define ex-4.24-test
  '(begin
     (define (loop x)
       (if (= x 0)
	   'done
	   (begin
	     (loop (- x 1)))))
     (loop 10000)))

; (time (lambda () (analyzing-eval ex-4.24-test the-global-environment)))

; time = 0.8400000000000016

; (time (lambda () (eval ex-4.24-test the-global-environment)))

; time = 5.229999999999999

; 6.2 times speedup

(define ex-4.24-fib
  '(begin
     (define (fib x)
       (if (= x 0)
	   0
	   (if (= x 1)
	       1
	       (+ (fib (- x 1)) (fib (- x 2))))))
     (fib 20)))

; (time (lambda () (analyzing-eval ex-4.24-fib the-global-environment)))

; time = 2.64

; (time (lambda () (eval ex-4.24-fib the-global-environment)))

; time = 15.13

; 5.7 times speedup

(define ex-4.24-let
  '(begin
     (define (fib x)
       (if (= x 0)
	   0
	   (if (= x 1)
	       1
	       (let ((prev (fib (- x 1)))
		     (prev-prev (fib (- x 2))))
		 (+ prev prev-prev)))))
     (fib 20)))

; (time (lambda () (analyzing-eval ex-4.24-let the-global-environment)))

; time = 3.030000000000001

; (time (lambda () (eval ex-4.24-let the-global-environment)))

; time = 17.599999999999998

; 5.8 times speedup

;;;;;;;;;;;;;;;;;
;;; Exercise 4.25
;;;;;;;;;;;;;;;;;

; What happens if we attempt to evaluate (factorial 5)?

; infinite loop happens, because even if (= n 1)
; (* n (factorial (- n 1))) is evaluated anyway

;Will our definitions work in a normal-order language?

; Yes! Because if (= n 1)
; evaluation of (* n (factorial (- n 1))) will be delayed

;;;;;;;;;;;;;;;;;
;;; Exercise 4.26
;;;;;;;;;;;;;;;;;

; Show how to implement unless as a derived expression (like cond or let):

(define (unless? exp) (tagged-list? exp 'unless))
(define (unless-predicate exp) (cadr exp))
(define (unless-consequent exp) (caddr exp))
(define (unless-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (eval-unless exp env)
  (if (false? (eval (unless-predicate exp) env))
      (eval (unless-consequent exp) env)
      (eval (unless-alternative exp) env)))

(add-eval-rule unless? eval-unless)

; give an example of a situation where it might be useful to have
; unless available as a procedure, rather than as a special form.

; if "if" and "unless" were functions we could write more literate
; programs, for example consider normal-order procedure: 

; (define (do-something predicate condition)
;   (predicate condition actualy-do-something do-nothing))

; (go-to-bar if (> money 100.0))

; (go-to-bar unless (wife-objects?))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.27
;;;;;;;;;;;;;;;;;

(load "lazy")

(define the-global-environment (setup-environment))

;; ;;; L-Eval input:
;; (define count 0)
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (define (id x)
;;   (set! count (+ count 1))
;;   x)
;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; (define w (id (id 10)))	

; inner (id 10) is delayed, outer (id ...) is evaluated,
; thus increasing count by 1, w is assigned delayed (id 10)

;; ;;; L-Eval value:
;; ok
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 1
;; ;;; L-Eval input:
;; w

; w was delayed (id 10) and now by accessing we are forcing it
; forcing of (id 10) evaluated it's body, thus increasing count by 1 more

;; ;;; L-Eval value:
;; 10
;; ;;; L-Eval input:
;; count
;; ;;; L-Eval value:
;; 2

;;;;;;;;;;;;;;;;;
;;; Exercise 4.28
;;;;;;;;;;;;;;;;;

;(define (x-fn)
;  +)

;(define (y-fn a)
;  a)

; (eval '((y-fn (x-fn)) 5 6) env)
; would yield operator that is delayed call to x-fn function

; (actual-value '((y-fn (x-fn)) 5 6) env)
; would force delayed call to x-fn thus yielding operator +

;;;;;;;;;;;;;;;;;
;;; Exercise 4.29
;;;;;;;;;;;;;;;;;

(define ex-4.29-prog
  '(begin
     (define (raise-to-power base power)
       (cond ((= power 0) 1)
	     ((= 0 (remainder power 2))
	      (* (raise-to-power base (/ power 2))
		 (raise-to-power base (/ power 2))))
	     (else (* base (raise-to-power base (- power 1))))))
     (define (fib n)
       (cond ((= n 0) 0)
	     ((= n 1) 1)
	     (else (+ (fib (- n 1)) (fib (- n 2))))))
     (raise-to-power (fib 15) 11)))

; with memoization
; (time (lambda () (eval ex-4.29-prog the-global-environment)))

; time = 1.03

; without memoization
; (time (lambda () (eval ex-4.29-prog the-global-environment)))

; time = 41.06

; (define (square x)
;   (* x x))
;;; L-Eval input:
; (square (id 10))
;;; L-Eval value:
; <response>
; with-memoization = 100
; without-memoization = 100
;;; L-Eval input:
; count
; ;;; L-Eval value:
; <response> 
; with-memoization = 1
; without-memoization = 2

;;;;;;;;;;;;;;;;;
;;; Exercise 4.30
;;;;;;;;;;;;;;;;;

; A.
; lambda and begin are not procedures, but special forms,
; therefore expressions in body are not viewed as arguments,
; evaluator doesn't force the expressions in a sequence,
; nevertheless it evaluates expressions in a sequence 
; by applying all procedures, it's just some arguments may
; not be evaluated and return value could be a thunk

; B.
; What are the values of (p1 1) and (p2 1) with the original eval-sequence?
; (p1 1) = (1 2)
; (p2 1) = 1

; What would the values be with Cy's proposed change to eval-sequence?
; (p1 1) = (1 2)
; (p2 1) = (1 2)

; C.
; Because Cy's eval-sequence in exmaple A
; does the same thing with more computations

; for example if we have list of thunks then running for-each with id function
; would count elements in list in count variable, difference is
; that Cy's eval-sequence would force all thunks in that list,
; exercise A list contains numbers and forcing numbers has no side effects

; D.
; I think sequences should be treated as they were in text, because
; whole thing about lazy evaluation is efficiency gained by not evaluating
; certain expressions, I don't like Cy's approach because imposes more
; evaluations and solves problem only in some specific cases for example:

;(define (p2 x)
;  (define (p e)
;    (set! e e)
;    x) 
;  (p (set! x (cons x '(2)))))

; has (p2 1) = 1 with Cy's eval-sequence too

; so stick with functional paradigm and 
; try to use as few evaluations as posible

;;;;;;;;;;;;;;;;;
;;; Exercise 4.31
;;;;;;;;;;;;;;;;;

(define (simple-thunk? obj)
  (tagged-list? obj 'simple-thunk))

(define (simple-delay-it exp env)
  (list 'simple-thunk exp env))

(define (smart-delay-it parm exp env)
  (cond ((not (pair? parm)) (eval exp env))
	((eq? 'lazy (cadr parm)) (simple-delay-it exp env))
	((eq? 'lazy-memo (cadr parm)) (delay-it exp env))
	(else (error "SMART-DELAY-IT unknown parameter type" parm))))

(define (get-parameter-names parameters)
  (map (lambda (x) (if (pair? x) (car x) x)) parameters))

(define (weak-apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))  ; changed
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (get-parameter-names (procedure-parameters procedure))
           (list-of-maybe-delayed
	    (procedure-parameters procedure)
	    (append arguments (procedure-implicit-args procedure))
	    env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- WEAK-APPLY" procedure))))

(define (list-of-maybe-delayed parms exps env)
  (if (no-operands? exps)
      '()
      (let ((x (smart-delay-it (car parms) (first-operand exps) env)))
	(cons x (list-of-maybe-delayed (cdr parms) (rest-operands exps) env)))))

(define (force-it obj)
  (if (simple-thunk? obj)
      (actual-value (thunk-exp obj) (thunk-env obj))
      (force-it-memoization obj)))

(assert
 '(eval '(begin
	   (define log '())
	   (define (dbg sym x) 
	     (set! log (append log (list sym)))
	     x)
	   (define (funny-adder x (y lazy) (z lazy-memo))
	     (dbg 'funny-adder 0)
	     (+ x y y z z))
	   (funny-adder (dbg 'x-var 1) (dbg 'y-var 2) (dbg 'z-var 3))
	   log)
	the-global-environment)
 '(x-var	; first we evaluate x argument when calling funny-adder
   funny-adder  ; then we enter procedure funny adder
   y-var	; evaluate y twice because it is not memoized
   y-var
   z-var))	; we evaluate z-var once because second time it is memoized

;;;;;;;;;;;;;;;;;
;;; Exercise 4.32
;;;;;;;;;;;;;;;;;

; Give some examples that illustrate the difference between the streams
; of chapter 3 and the ``lazier'' lazy lists described in this section.
; How can you take advantage of this extra laziness?

; for finite streams for example (length ...) procedure that counts elements
; in stream would evaluate all stream car elements, but lazy lists would
; just count elements in list without evaluating elements

; if stream elements do not depend on each other then list-ref also
; would just evaluate n-th element in lazy list case, but
; evaluate all n elements form begining in stream case

;;;;;;;;;;;;;;;;;
;;; Exercise 4.33
;;;;;;;;;;;;;;;;;

(load "lazy")

(define the-global-environment (setup-environment))

(define (text-of-quotation exp env) 
  (let ((text (cadr exp)))
    (if (pair? text)
	(delay-it (list 'cons
			(list 'quote (car text))
			(list 'quote (cdr text)))
		  env)
	text)))

(eval 
 '(begin
    (define (cons x y)
      (lambda (m) (m x y)))

    (define (car z)
      (z (lambda (p q) p)))
    
    (define (cdr z)
      (z (lambda (p q) q))))
 the-global-environment)

(assert '(actual-value '(car '(a b c)) the-global-environment) 'a)

(assert '(actual-value '(car (cdr '(a b c))) the-global-environment) 'b)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.34
;;;;;;;;;;;;;;;;;

(define (cons-object? object)
  (tagged-list? object 'cons))

(define car-get
  (list '(lambda (p q) p)))

(define cdr-get 
  (list '(lambda (p q) q)))

(define (get-value proc args)
  (force-it (weak-apply (cdr proc) args the-global-environment)))

(define (print-cons-object object depth)
  (let ((x (get-value object car-get))
	(y (get-value object cdr-get)))
    (user-print x (- depth 1))
    (cond ((= depth 0) (display " ..."))
	  ((null? y) 'nothing)
	  ((cons-object? y)
	   (display " ")
	   (print-cons-object y (- depth 1)))
	  (else (display " . ")
		(user-print y (- depth 1))))))

(define user-print-depth 10) ; specify -1 to print lists to infinity and beyond

(define (get-count count)
  (if (null? count)
      user-print-depth
      (car count)))

(define (user-print object . tail)
  (cond ((cons-object? object)
	 (display "(")
	 (print-cons-object object (get-count tail))
	 (display ")"))
	((compound-procedure? object)
	 (display (list 'compound-procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>)))
	(else (display object))))

(actual-value
 '(begin
    (define (cons x y)
      (glue 'cons (lambda (m) (m x y))))

    (define (car z)
      ((rest z) (lambda (p q) p)))

    (define (cdr z)
      ((rest z) (lambda (p q) q))))
 the-global-environment)

(define (test-4.34)
  (define exp-1 ''(a b c b c d e f g h i j k))
  (define exp-2 ''(a (b (c (d (e (f (g (h (i (j (k))))))))))))
  (user-print (actual-value exp-1 the-global-environment))
  (newline)
  (user-print (actual-value exp-2 the-global-environment))
  (newline))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.35
;;;;;;;;;;;;;;;;;

(load "amb")

(define (eval-amb exp)
  (ambeval exp the-global-environment
	   (lambda (val next-alternative) val)
	   (lambda () 'epic-fail)))

(eval-amb
 '(define (require p)
    (if (not p) (amb))))

(eval-amb
 '(define (a-pythagorean-triple-between low high)
    (let ((i (an-integer-between low high)))
      (let ((j (an-integer-between i high)))
	(let ((k (an-integer-between j high)))
	  (require (= (+ (* i i) (* j j)) (* k k)))
	  (list i j k))))))

(eval-amb
 '(define (an-integer-between low high)
    (cond ((> low high) (amb))
	  ((= low high) (amb low))
	  (else (amb low (an-integer-between (+ low 1) high))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.36
;;;;;;;;;;;;;;;;;

; Explain why simply replacing an-integer-between by 
; an-integer-starting-from in the procedure in exercise 4.35
; is not an adequate way to generate arbitrary Pythagorean triples.

; because it will sequentaly try triples:

; (1 1 1)
; (1 1 2)
; (1 1 3)
; ...
; (1 1 n)
; ...
; (1 1 ad-infinitum)

; and will never get to increasing first two numbers
; it is somewhat same problem that was with streams
; in section 3.5.3 (Infinite streams of pairs)
; that led to procedure "interleave"

(eval-amb 
 '(define (an-integer-starting-from n)
    (amb n (an-integer-starting-from (+ n 1)))))
    
(eval-amb
 '(define (a-pythagorean-triple-starting-from n)
    (let ((k (an-integer-starting-from n)))
      (let ((j (an-integer-between n k)))
	(let ((i (an-integer-between n j)))
	  (require (= (+ (* i i) (* j j)) (* k k)))
	  (list i j k))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.37
;;;;;;;;;;;;;;;;;

; Ben Bitdiddle claims that the following method for generating Pythagorean
; triples is more efficient than the one in exercise 4.35. Is he correct?

; Yes, he is correct, because method in exercise 4.35 is cubic, 
; it searches all three numbers of triple through range of integers

; While Ben's method is quadratic, because it searches only two
; numbers through range of integers, thrid value is simply calculated
; in constant time and then tested wether it's square root is integer

;;;;;;;;;;;;;;;;;
;;; Exercise 4.38
;;;;;;;;;;;;;;;;;

(eval-amb 
 '(define (distinct? items)
    (cond ((null? items) true)
	  ((null? (cdr items)) true)
	  ((member (car items) (cdr items)) false)
	  (else (distinct? (cdr items))))))

(eval-amb
 '(define (multiple-dwelling)
    (let ((baker (amb 1 2 3 4 5))
	  (cooper (amb 1 2 3 4 5))
	  (fletcher (amb 1 2 3 4 5))
	  (miller (amb 1 2 3 4 5))
	  (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith)))))

; How many solutions are there to this modified puzzle?
; five!

; ((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))
; ((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))
; ((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))
; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))
; ((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.39
;;;;;;;;;;;;;;;;;

; Does the order of the restrictions in the 
; multiple-dwelling procedure affect the answer?

; No!

; Does it affect the time to find an answer?

; No!

; Thing is that there are 5^5 possible combinations that are all 
; generated by amb, then series of tests are run on every single
; combination. When any of tests fail combination is discarded.
; All tests run in constant time. Therefore there is no difference
; in speed or in answer wether we reorder tests or not.

;;;;;;;;;;;;;;;;;
;;; Exercise 4.40
;;;;;;;;;;;;;;;;;

; In the multiple dwelling problem, how many sets of assignments 
; are there of people to floors, both before and after the
; requirement that floor assignments be distinct?

; before = 5^5 = 3125

; after = 5! = 120

(eval-amb
 '(define (multiple-dwelling)
    ; putting fletcher first because it has most restrictions
    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (let ((cooper (amb 1 2 3 4 5)))
	(require (not (= cooper 1)))	
	(require (not (= cooper fletcher)))
	(require (not (= (abs (- fletcher cooper)) 1)))	 	  
	(let ((baker (amb 1 2 3 4 5)))
	  (require (not (= baker 5)))
	  (require (not (= baker fletcher)))
	  (require (not (= baker cooper)))
	  (let ((miller (amb 1 2 3 4 5)))
	    (require (> miller cooper))
	    (require (not (= miller baker)))
	    (require (not (= miller cooper)))
	    (require (not (= miller fletcher)))
	    (let ((smith (amb 1 2 3 4 5)))
	      (require (not (= smith baker)))
	      (require (not (= smith cooper)))
	      (require (not (= smith fletcher)))
	      (require (not (= smith miller)))
	      (require (not (= (abs (- smith fletcher)) 1)))
	      (list (list 'baker baker)
		    (list 'cooper cooper)
		    (list 'fletcher fletcher)
		    (list 'miller miller)
		    (list 'smith smith)))))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.41
;;;;;;;;;;;;;;;;;

(define (n-times fn n)
  (define (loop i)
    (fn i)
    (if (< i n)
	(loop (+ i 1))))
  (loop 1))

(define (distinct? items)
  (cond ((null? items) true)
	((null? (cdr items)) true)
	((member (car items) (cdr items)) false)
	(else (distinct? (cdr items)))))

(define (test-dwellers baker cooper fletcher smith miller)
  (and (distinct? (list baker cooper fletcher miller smith))
       (not (= (abs (- smith fletcher)) 1))
       (not (= (abs (- fletcher cooper)) 1))
       (not (= baker 5))
       (not (= cooper 1))
       (not (= fletcher 5))
       (not (= fletcher 1))
       (> miller cooper)))

(define (make-dwellers baker cooper fletcher smith miller)
  (list (list 'baker baker)
	(list 'cooper cooper)
	(list 'fletcher fletcher)
	(list 'miller miller)
	(list 'smith smith)))

(define (solve-multiple-dwelling)
  (let ((answers nil))
    (n-times
     (lambda (baker)
       (n-times
	(lambda (cooper)
	  (n-times
	   (lambda (fletcher)
	     (n-times
	      (lambda (smith)
		(n-times
		 (lambda (miller)
		   (let ((dwellers (list baker cooper fletcher smith miller)))
		     (if (apply test-dwellers dwellers)
			 (let ((answer (apply make-dwellers dwellers)))
			   (set! answers (cons answer answers))))))
		 5))
	      5))
	   5))
	5))
     5)
    answers))

(assert '(solve-multiple-dwelling)
	'(((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.42
;;;;;;;;;;;;;;;;;

(eval-amb
 '(define (liars-puzzle)
    (define (true-or-false x y)
      (amb (begin (require x)
		  (require (not y)))
	   (begin (require (not x))
		  (require y))))	   
    (let ((betty (amb 1 2 3 4 5))
	  (ethel (amb 1 2 3 4 5))
	  (joan  (amb 1 2 3 4 5))
	  (kitty (amb 1 2 3 4 5))
	  (mary  (amb 1 2 3 4 5)))
      (require (distinct? (list betty ethel joan kitty mary)))
      (true-or-false (= kitty 2) (= betty 3))
      (true-or-false (= ethel 1) (= joan 2))
      (true-or-false (= joan 3) (= ethel 5))
      (true-or-false (= kitty 2) (= mary 4))
      (true-or-false (= mary 4) (= betty 5))
      (list (list 'betty betty)
	    (list 'ethel ethel)
	    (list 'joan joan)
	    (list 'kitty kitty)
	    (list 'mary mary)))))

; ((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.43
;;;;;;;;;;;;;;;;;

(eval-amb
 '(define (find predicate list)
    (cond ((null? list) false)
	  ((predicate (car list)) (car list))
	  (else (find predicate (cdr list))))))

(eval-amb
 '(define (map proc list)
    (if (null? list) 
	'()
	(cons (proc (car list)) (map proc (cdr list))))))

(eval-amb
 '(define (problematical-recreations original)
    (define (daughter x)  
      (car x))
    (define (father x) 
      (car (cdr x)))
    (define (yacht-owner x)
      (car (cdr (cdr x))))
    (define (is fn name)
      (lambda (x) (eq? name (fn x))))
    (define (choice) 
      (if original
	  (amb 'dr-parker 'mr-hall 'downing)
	  (amb 'dr-parker 'mr-hall 'downing 'mr-moore)))
    (define (ann-dad)
      (if original 'mr-moore (choice)))

                        ; daughter    father   yacht-owner
    (let ((db (list (list 'lorna     (choice)  'mr-moore)
		    (list 'mary-ann  (ann-dad) 'dr-parker)
		    (list 'rosalind  (choice)  'mr-hall)
		    (list 'melissa   'barnacle 'downing)
		    (list 'gabrielle (choice)  'barnacle))))

      (require (distinct? (map father db)))
      
      (let ((gabrielle-father (father (find (is daughter 'gabrielle) db))))
	(require (eq? (daughter (find (is yacht-owner gabrielle-father) db))
		      (daughter (find (is father 'dr-parker) db)))))

      (if original
	  (father (find (is daughter 'lorna) db))
	  db))))

; lornas father is downing

; Also determine how many solutions there are if 
; we are not told that Mary Ann's last name is Moore.

; there are four (4) solutions

;((lorna     dr-parker mr-moore)
; (mary-ann  mr-hall   dr-parker)
; (rosalind  downing   mr-hall)
; (melissa   barnacle  downing)
; (gabrielle mr-moore  barnacle))

;((lorna     dr-parker  mr-moore)
; (mary-ann  downing    dr-parker)
; (rosalind  mr-hall    mr-hall)
; (melissa   barnacle   downing)
; (gabrielle mr-moore   barnacle))

;((lorna     downing   mr-moore)
; (mary-ann  mr-moore  dr-parker)
; (rosalind  dr-parker mr-hall)
; (melissa   barnacle  downing)
; (gabrielle mr-hall b arnacle))

;((lorna     mr-moore  mr-moore)
; (mary-ann  downing   dr-parker)
; (rosalind  dr-parker mr-hall)
; (melissa   barnacle  downing)
; (gabrielle mr-hall   barnacle))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.44
;;;;;;;;;;;;;;;;;

(define (is-safe? row1 k rowk)
  (and (not (= row1 rowk))
       (not (= k (abs (- rowk row1))))))

(define (safe? new positions)
  (or (null? positions)
      (and (is-safe? (car positions) (length positions) new)	    
	   (safe? new (cdr positions)))))

(add-primitive 'safe?)

(eval-amb
 '(define (at-end seq elem)
    (if (null? seq)
	(list elem)
	(cons (car seq) (at-end (cdr seq) elem)))))

(eval-amb
 '(define (find-queens n)
    (define (queen-loop i board)
	(if (= n i)
	    board
	    (let ((candidate (an-integer-between 1 n)))	
	      (require (safe? candidate board))
	      (queen-loop (+ i 1) (at-end board candidate)))))
    (queen-loop 0 '())))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.45
;;;;;;;;;;;;;;;;;

(eval-amb 
 '(begin
    (define nouns '(noun student professor cat class fork))
    (define verbs '(verb studies lectures eats sleeps))
    (define prepositions '(prep for to in by with))
    (define articles '(article the a))

    (define *unparsed* '())
    (define (parse input)
      (set! *unparsed* input)
      (let ((sent (parse-sentence)))
	(require (null? *unparsed*))
	sent))

    (define (parse-word word-list)
      (require (not (null? *unparsed*)))
      (require (memq (car *unparsed*) (cdr word-list)))
      (let ((found-word (car *unparsed*)))
	(set! *unparsed* (cdr *unparsed*))
	(list (car word-list) found-word)))

    (define (parse-sentence)
      (list 'sentence
	    (parse-noun-phrase)
	    (parse-verb-phrase)))
    
    (define (parse-simple-noun-phrase)
      (list 'simple-noun-phrase
	    (parse-word articles)
	    (parse-word nouns)))

    (define (parse-prepositional-phrase)
      (list 'prep-phrase
	    (parse-word prepositions)
	    (parse-noun-phrase)))

    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
	(amb verb-phrase
	     (maybe-extend (list 'verb-phrase
				 verb-phrase
				 (parse-prepositional-phrase)))))
      (maybe-extend (parse-word verbs)))
    
    (define (parse-noun-phrase)
      (define (maybe-extend noun-phrase)
	(amb noun-phrase
	     (maybe-extend (list 'noun-phrase
				 noun-phrase
				 (parse-prepositional-phrase)))))
      (maybe-extend (parse-simple-noun-phrase)))))

; (parse '(The professor lectures to the student in the class with the cat))

; 1.

;(sentence
; (simple-noun-phrase (article the) (noun professor))
; (verb-phrase
;  (verb-phrase
;   (verb-phrase
;    (verb lectures)
;    (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;   (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;  (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

; the professor lectures to the student
; the professor lectures in the class 
; the professor lectures with the cat

; 2.

;(sentence
; (simple-noun-phrase (article the) (noun professor))
; (verb-phrase 
;  (verb-phrase
;   (verb lectures)
;   (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;  (prep-phrase 
;   (prep in) 
;   (noun-phrase 
;    (simple-noun-phrase (article the) (noun class))
;    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

; the professor lectures to the student
; the professor lectures in the class 
; lecture takes place in the class with the cat

; 3.
;(sentence
; (simple-noun-phrase (article the) (noun professor))
; (verb-phrase
;  (verb-phrase
;   (verb lectures)
;   (prep-phrase 
;    (prep to)
;    (noun-phrase 
;     (simple-noun-phrase (article the) (noun student)) 
;     (prep-phrase 
;      (prep in)
;      (simple-noun-phrase (article the) (noun class))))))
;  (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))

; the professor lectures to the student
; the student is in the class
; the professor lectures with the cat

; 4.
;(sentence
; (simple-noun-phrase (article the) (noun professor))
; (verb-phrase
;  (verb lectures)
;  (prep-phrase
;   (prep to)
;   (noun-phrase
;    (noun-phrase
;     (simple-noun-phrase (article the) (noun student))
;     (prep-phrase 
;      (prep in) 
;      (simple-noun-phrase (article the) (noun class))))
;    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))

; the professor lectures to the student
; the student is in the class
; the student has the cat with him

; 5.
;(sentence
; (simple-noun-phrase (article the) (noun professor))
; (verb-phrase
;  (verb lectures)
;  (prep-phrase 
;   (prep to)
;   (noun-phrase
;    (simple-noun-phrase (article the) (noun student))
;    (prep-phrase 
;     (prep in) 
;     (noun-phrase
;      (simple-noun-phrase (article the) (noun class))
;      (prep-phrase
;       (prep with)
;       (simple-noun-phrase (article the) (noun cat)))))))))

; the professor lectures to the student
; the student is in the class
; lecture takes place in the class with the cat

;;;;;;;;;;;;;;;;;
;;; Exercise 4.46
;;;;;;;;;;;;;;;;;

; consider previous simple grammar 

; (define (parse-sentence)
;  (list 'sentence
;         (parse-noun-phrase)
;         (parse-word verbs)))

; and sentence 'the cat eats'
; left to right evaluator parses it alright

; on other hand if evaluation were from right to left,
; then (parse-word verbs) would be evaluated first
; (require (memq (car *unparsed*) (cdr word-list)))
; will fail because 'the' is not in verb list
; and amb would signal that it is impossible to parse this sentence

; we have these woes because we have global state variable *unparsed*
; and that makes order of execution relevant

;;;;;;;;;;;;;;;;;
;;; Exercise 4.47
;;;;;;;;;;;;;;;;;

;(define (parse-verb-phrase)
;  (amb (parse-word verbs)
;       (list 'verb-phrase
; 	     (parse-verb-phrase)
;	     (parse-prepositional-phrase)))))

; Does this work?

; No, because it goes into infinite loop every time it tries second amb branch
; Although before going into infinite loop it may print some good answers

; Does the program's behavior change if we
; interchange the order of expressions in the amb? 

; Yes, program behavior changes in that it does not go into infinite loop
; anymore, because by evaluating (parse-prepositional-phrase) before
; (parse-verb-phrase) whould eventually shorten *unparsed* until failure,
; but it essentially changes grammar parsing orderer.

; for example verb phrase 'eat with cat'
; first branch will consume 'eat' and then fail 
; because there would be 'with cat' left unparsed
; second branch will fail because by evaluating
; (parse-prepositional-phrase) program will
; search for preposition and fail because 'eat' is not

;;;;;;;;;;;;;;;;;
;;; Exercise 4.48
;;;;;;;;;;;;;;;;;

(eval-amb 
 '(begin
    (define adjectives '(adjective ugly stupid lazy dirty shitty))
    (define adverbs '(adverb hastily ignorantly))
    (define coordinators '(coordinator for and nor but or yet so))
    (define conjunct '(conjunct and))

    (define (parse-adjective-phrase)
      (amb (parse-word adjectives)
	   (list 'adjective-phrase
		 (parse-word adjectives)
		 (parse-word conjunct)
		 (parse-adjective-phrase))))

    (define (parse-simple-noun-phrase)      
      (amb (list 'simple-noun-phrase
		 (parse-word articles)
		 (parse-word nouns))
	   (list 'simple-noun-phrase
		 (parse-word articles)
		 (parse-adjective-phrase)
		 (parse-word nouns))))

    (define (parse-simple-sentence)
      (list 'simple-sentence
	    (parse-noun-phrase)
	    (parse-verb-phrase)))

    (define (parse-adverb-with-verb)
      (amb (parse-word verbs)
	   (list 'adverb-verb-phrase
		 (parse-word adverbs)
		 (parse-word verbs))))

    (define (parse-sentence)
      (amb (parse-simple-sentence)	   
	   (list 'compound-sentence
		 (parse-simple-sentence)
		 (parse-word coordinators)
		 (parse-sentence))))

    (define (parse-verb-phrase)
      (define (maybe-extend verb-phrase)
	(amb verb-phrase
	     (maybe-extend 
	      (list 'verb-phrase verb-phrase (parse-prepositional-phrase)))))
      (maybe-extend (parse-adverb-with-verb)))))

;(parse '(the ugly and lazy cat hastily eats with the dirty fork
;         but the stupid student ignorantly sleeps in the shitty class))

;(compound-sentence
; (simple-sentence
;  (simple-noun-phrase 
;   (article the)
;   (adjective-phrase (adjective ugly) (conjunct and) (adjective lazy))
;   (noun cat))
;  (verb-phrase
;   (adverb-verb-phrase (adverb hastily) (verb eats))
;   (prep-phrase 
;    (prep with)
;    (simple-noun-phrase (article the) (adjective dirty) (noun fork))))) 
;
; (coordinator but) 
; 
; (simple-sentence
;  (simple-noun-phrase
;   (article the)
;   (adjective stupid)
;   (noun student))
;  (verb-phrase 
;   (adverb-verb-phrase (adverb ignorantly) (verb sleeps))
;   (prep-phrase 
;    (prep in)
;    (simple-noun-phrase (article the) (adjective shitty) (noun class))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.49
;;;;;;;;;;;;;;;;;

(eval-amb 
 '(begin
    (define (amb-list seq)
      (cond ((null? seq) (amb))
	    ((null? (cdr seq)) (amb (car seq)))
	    (else (amb (car seq) (amb-list (cdr seq))))))

    (define (parse-word word-list)
      (list (car word-list) (amb-list (cdr word-list))))))

; the student studies
; the student studies for the student
; the student studies for the student for the student
; the student studies for the student for the student for the student

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For personal curiosity
;;;;;;;;;;;;;;;;;;;;;;;;;;

; collect all possible answers in list

(define (eval-amb-in-list exp)
  (ambeval exp the-global-environment
	   (lambda (val next-alternative) 
	     (cons val (next-alternative)))
	   (lambda () '())))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.50
;;;;;;;;;;;;;;;;;

(define (with-out-elt seq num)
  (if (= num 0)
      (cdr seq)
      (cons (car seq) (with-out-elt (cdr seq) (- num 1)))))

; just replaced analyze-amb with this analyze-ramb in amb.scm to test it
; because otherwise all grammar generation ambs should be changed to rambs
(define (analyze-ramb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
	(let ((victim (random (length choices))))
	  (if (null? choices)
	      (fail)
	      ((list-ref choices victim)
	       env succeed
	       (lambda ()
		 (try-next (with-out-elt choices victim)))))))
      (try-next cprocs))))

(define (convert-to-english sentence)
  (cond ((symbol? sentence) '())
	((and (= (length sentence) 2)
	      (symbol? (first sentence))
	      (symbol? (second sentence)))
	 (list (second sentence)))	
	(else (apply append (map convert-to-english sentence)))))

; (convert-to-english (eval-amb '(parse '())))

; many sentences were over hundred words, some more than 
; thousand words so I picked most interesting and short ones
; also the way I wrote exercise 4.49 results in that all words
; are not picked with equal probability

(define (shuffle-list seq)
  (if (null? seq)
      '()
      (let ((victim (random (length seq))))
	(cons (list-ref seq victim)
	      (shuffle-list (with-out-elt seq victim))))))

(add-primitive 'shuffle-list)

; this should alleviate word probability problem
(eval-amb
 '(define (parse-word word-list)
    (let ((shuffled-words (shuffle word-list)))
      (list (car shuffled-words) (amb-list (cdr shuffled-words))))))

; the professor ignorantly studies

; the cat studies for a professor lectures and a student studies

; the class hastily studies for a ugly student

; the fork ignorantly eats

; the ugly cat hastily studies

; a lazy and ugly professor studies to the ugly cat with a fork

; a ugly and ugly and ugly and dirty and shitty professor hastily studies

; a professor eats for the student to the ugly cat

; the stupid and ugly student hastily sleeps for a class ignorantly studies

; the ugly student ignorantly eats

;;;;;;;;;;;;;;;;;
;;; Exercise 4.51
;;;;;;;;;;;;;;;;;

(eval-amb
 '(define (an-element-of items)
    (require (not (null? items)))
    (amb (car items) (an-element-of (cdr items)))))

(assert 
 '(eval-amb-in-list
   '(begin
      (define count 0)
      (let ((x (an-element-of '(a b c)))
	    (y (an-element-of '(a b c))))
	(permanent-set! count (+ count 1))
	(require (not (eq? x y)))
	(list x y count))))
 '((a b 2) (a c 3) (b a 4) (b c 6) (c a 7) (c b 8)))

; What values would have been displayed if we had used set! here
; rather than permanent-set! ?

; ((a b 1) (a c 1) (b a 1) (b c 1) (c a 1) (c b 1))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.52
;;;;;;;;;;;;;;;;;

; look into amb.scm

(add-primitive 'even?)

(assert 
 '(eval-amb 
   '(if-fail (let ((x (an-element-of '(1 3 5))))
	       (require (even? x))
	       x)
	     'all-odd))
 'all-odd)

(assert 
 '(eval-amb
   '(if-fail (let ((x (an-element-of '(1 3 5 8))))
	       (require (even? x))
	       x)
	     'all-odd))
 8)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.53
;;;;;;;;;;;;;;;;;

(define (prime? n)
  (define (smallest-divisor n)
    (find-divisor n 2))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
	  ((divides? test-divisor n) test-divisor)
	  (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(add-primitive 'prime?)

(eval-amb
 '(define (prime-sum-pair list1 list2)
    (let ((a (an-element-of list1))
	  (b (an-element-of list2)))
      (require (prime? (+ a b)))
      (list a b))))

(assert
 '(eval-amb-in-list
   '(let ((pairs '()))
      (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
		 (permanent-set! pairs (cons p pairs))
		 (amb))
	       pairs)))
 '(((8 35) (3 110) (3 20))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.54
;;;;;;;;;;;;;;;;;

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not pred-value)
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.55
;;;;;;;;;;;;;;;;;

(define eval builtin-eval)

(load "query")

(initialize-data-base microshaft-data-base)

; A.
; (supervisor ?x (Bitdiddle Ben))

; B.
; (job ?x (accounting . ?y))

; C.
; (address ?x (Slumerville . ?y))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.56
;;;;;;;;;;;;;;;;;

; A.
;(and (supervisor ?x (Bitdiddle Ben))
;     (address ?x ?where))

; B.
;(and (salary ?x ?x-money)
;     (salary (Bitdiddle Ben) ?ben-money)
;     (lisp-value > ?ben-money ?x-money))

; C.
;(and (supervisor ?x ?y)
;     (not (job ?y (computer . ?z)))
;     (job ?y ?w))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.57
;;;;;;;;;;;;;;;;;

(query-eval
 '(rule (can-replace ?p1 ?p2)
	(and (job ?p1 ?p1-job)
	     (job ?p2 ?p2-job)
	     (not (same ?p1 ?p2))
             ; if either person 1 does the same job as person 2
	     (or (same ?p1-job ?p2-job)
		 (and (job ?someone ?someones-job)
		      ; or someone who does persons 1's job
		      (job ?p1 ?someones-job) 
		      ; can also do person 2's job
		      (can-do-job ?someones-job ?p2-job))))))

; a. all people who can replace Cy D. Fect;

(assert '(query-eval '(can-replace ?person (Fect Cy D)))
	'((can-replace (hacker alyssa p) (fect cy d))
	  (can-replace (bitdiddle ben) (fect cy d))))

; b. all people who can replace someone who is being paid more
;    than they are, together with the two salaries.

(assert 
 '(query-eval
   '(and (can-replace ?p1 ?p2)
	 (salary ?p1 ?p1-salary)
	 (salary ?p2 ?p2-salary)
	 (lisp-value < ?p1-salary ?p2-salary)))
 '((and (can-replace (fect cy d) (hacker alyssa p))
	(salary (fect cy d) 35000)
	(salary (hacker alyssa p) 40000)
	(lisp-value < 35000 40000))
   (and (can-replace (aull dewitt) (warbucks oliver))
	(salary (aull dewitt) 25000)
	(salary (warbucks oliver) 150000)
	(lisp-value < 25000 150000))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.58
;;;;;;;;;;;;;;;;;

; Define a rule that says that a person is a "big shot"
; in a division if the person works in the division but does
; not have a supervisor who works in the division.

; initialy I had a bug, by adding Uber Ben 
; as Bitdiddle Ben's supervisor exposed that bug

; (query-eval '(assert! (job (Uber Ben) (computer programmer))))
; (query-eval '(assert! (supervisor (Bitdiddle Ben) (Uber Ben))))

(query-eval
 '(rule (big-shot ?person)
	(and (job ?person (?division . ?x))	     
	     (not (and (supervisor ?person ?boss)
		       (job ?boss (?division . ?y)))))))

(assert '(query-eval '(big-shot ?x))
	'((big-shot (scrooge eben))
	  (big-shot (warbucks oliver))	  
	  (big-shot (bitdiddle ben))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.59
;;;;;;;;;;;;;;;;;

(query-eval '(assert! (meeting accounting (Monday 9am))))
(query-eval '(assert! (meeting administration (Monday 10am))))
(query-eval '(assert! (meeting computer (Wednesday 3pm))))
(query-eval '(assert! (meeting administration (Friday 1pm))))

(query-eval '(assert! (meeting whole-company (Wednesday 4pm))))

; a. On Friday morning, Ben wants to query the data base for all
;    the meetings that occur that day. What query should he use?

(assert '(query-eval '(meeting ?division (Friday . ?time)))
	'((meeting administration (Friday 1pm))))

; b.
(query-eval '(rule (meeting-time ?person ?day-and-time)
		   (and (job ?person (?division . ?x))
			(or (meeting ?division ?day-and-time)
			    (meeting whole-company ?day-and-time)))))

; c. 
(assert '(query-eval '(meeting-time (Hacker Alyssa P) (Wednesday . ?time)))
	'((meeting-time (Hacker Alyssa P) (Wednesday 3pm))
	  (meeting-time (Hacker Alyssa P) (Wednesday 4pm))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.60
;;;;;;;;;;;;;;;;;

; On the other hand, when she tries to find all pairs of people who live near
; each other by querying (lives-near ?person-1 ?person-2) she notices that
; each pair of people who live near each other is listed twice; for example,
; (lives-near (Hacker Alyssa P) (Fect Cy D))
; (lives-near (Fect Cy D) (Hacker Alyssa P))
; Why does this happen?

; it happens because because system tests all persons in ?person-1 variable
; against all persons in ?person-2 variable and lives-near is commutative

; Is there a way to find a list of people who live near each other,
; in which each pair appears only once? Explain.

; this could be solved by assigning unique ID numbers to employees
; and then showing only those results where person-1 lives near
; person-2 that has higher ID number

(query-eval
 '(rule (lives-near-no-duplicates ?person-1 ?person-2)
	(and (address ?person-1 (?town . ?rest-1))
	     (address ?person-2 (?town . ?rest-2))
	     (not (same ?person-1 ?person-2))
	     (person-id ?person-1 ?pid1)
	     (person-id ?person-2 ?pid2)
	     (lisp-value > ?pid2 ?pid1))))

(assert '(query-eval '(lives-near-no-duplicates ?person-1 ?person-2))
	'((lives-near-no-duplicates (reasoner louis) (aull dewitt))
	  (lives-near-no-duplicates (hacker alyssa p) (fect cy d))
	  (lives-near-no-duplicates (bitdiddle ben) (aull dewitt))
	  (lives-near-no-duplicates (bitdiddle ben) (reasoner louis))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.61
;;;;;;;;;;;;;;;;;

(query-eval '(rule (?x next-to ?y in (?x ?y . ?u))))

(query-eval '(rule (?x next-to ?y in (?v . ?z))
		   (?x next-to ?y in ?z)))

(assert '(query-eval '(?x next-to ?y in (1 (2 3) 4)))
	'(((2 3) next-to 4 in (1 (2 3) 4))
	  (1 next-to (2 3) in (1 (2 3) 4))))

(assert '(query-eval '(?x next-to 1 in (2 1 3 1)))
	'((3 next-to 1 in (2 1 3 1))
	  (2 next-to 1 in (2 1 3 1))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.62
;;;;;;;;;;;;;;;;;

; Define rules to implement the last-pair operation of exercise 2.17,
; which returns a list containing the last element of a nonempty list.
; Check your rules on queries such as (last-pair (3) ?x),
; (last-pair (1 2 3) ?x), and (last-pair (2 ?x) (3)).

(query-eval '(rule (last-pair (?x . ?y) ?z) ; *1*
		   (last-pair ?y ?z)))

(query-eval '(rule (last-pair (?x) (?x))))  ; *2*

(assert '(query-eval '(last-pair (3) ?x))
	'((last-pair (3) (3))))

(assert '(query-eval '(last-pair (1 2 3) ?x))
	'((last-pair (1 2 3) (3))))

(assert '(query-eval '(last-pair (2 ?x) (3)))
	'((last-pair (2 3) (3))))

; Do your rules work correctly on queries such as (last-pair ?x (3)) ? 

; No, it went into infinite loop, if rule *2* were defined prior to *1*.

; If rule *1* is defined prior to *2*, then program prints:

; (last-pair (3) (3))
; (last-pair (?x-320 3) (3))
; (last-pair (?x-320 ?x-324 3) (3))
; (last-pair (?x-320 ?x-324 ?x-328 3) (3))
; (last-pair (?x-320 ?x-324 ?x-328 ?x-332 3) (3))
; ... Ad infinitum

;;;;;;;;;;;;;;;;;
;;; Exercise 4.63
;;;;;;;;;;;;;;;;;

(query-eval '(assert! (son Adam Cain)))
(query-eval '(assert! (son Cain Enoch)))
(query-eval '(assert! (son Enoch Irad)))
(query-eval '(assert! (son Irad Mehujael)))
(query-eval '(assert! (son Mehujael Methushael)))
(query-eval '(assert! (son Methushael Lamech)))
(query-eval '(assert! (wife Lamech Ada)))
(query-eval '(assert! (son Ada Jabal)))
(query-eval '(assert! (son Ada Jubal)))

(query-eval '(rule (grandson ?g ?s)
		   (and (son ?f ?s)
			(son ?g ?f))))

(query-eval '(rule (son ?f ?s)
		   (and (wife ?f ?w)
			(son ?w ?s))))

(assert '(query-eval '(grandson Cain ?s))
	'((grandson cain irad)))

(assert '(query-eval '(son Lamech ?s))
	'((son lamech jubal)
	  (son lamech jabal)))

(assert '(query-eval '(grandson Methushael ?s))
	'((grandson methushael jubal)
	  (grandson methushael jabal)))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.64
;;;;;;;;;;;;;;;;;

;(rule (outranked-by ?staff-person ?boss)
;      (or (supervisor ?staff-person ?boss)
;          (and (outranked-by ?middle-manager ?boss)
;               (supervisor ?staff-person ?middle-manager))))

; (outranked-by (Bitdiddle Ben) ?who)

; After answering, the system goes into an infinite loop.  Explain why.

; It is because first part of "or" is simple query that prints answer
; Second part of "or" and first part of "and" tries to apply rule
; outranked-by with both of it's variables unknown, in recursive
; appliction of outranked-by first part of "or" returns stream of
; some answers, but second part tries to apply outranked-by
; in exactly the same way as current rule application was done
; with both variables unknown so there it is - infinite loop

;;;;;;;;;;;;;;;;;
;;; Exercise 4.65
;;;;;;;;;;;;;;;;;

;(rule (wheel ?person)
;      (and (supervisor ?middle-manager ?person)
;           (supervisor ?x ?middle-manager)))

;(wheel ?who)

; Why is Oliver Warbucks listed four times?

; all supervisor database entries:
; (supervisor (Hacker Alyssa P) (Bitdiddle Ben))
; (supervisor (Fect Cy D) (Bitdiddle Ben))
; (supervisor (Tweakit Lem E) (Bitdiddle Ben))
; (supervisor (Reasoner Louis) (Hacker Alyssa P))
; (supervisor (Bitdiddle Ben) (Warbucks Oliver))
; (supervisor (Scrooge Eben) (Warbucks Oliver))
; (supervisor (Cratchet Robert) (Scrooge Eben))
; (supervisor (Aull DeWitt) (Warbucks Oliver))

; first supervisor query would generate one frame for each database
; supervisor entry because both variables ar unknown (?person == ?who):

; (?middle-manager = (Hacker Alyssa P)
;  ?person = (Bitdiddle Ben))

; (?middle-manager = (Fect Cy D) 
;  ?person = (Bitdiddle Ben))

; (?middle-manager = (Tweakit Lem E)
;  ?person = (Bitdiddle Ben))

; (?middle-manager = (Reasoner Louis)
;  ?person = (Hacker Alyssa P))

; (?middle-manager = (Bitdiddle Ben)
;  ?person = (Warbucks Oliver))

; (?middle-manager = (Scrooge Eben)
;  ?person = (Warbucks Oliver))

; (?middle-manager = (Cratchet Robert)
;  ?person = (Scrooge Eben))

; (?middle-manager = (Aull DeWitt)
;  ?person = (Warbucks Oliver))

; second supervisor query would sequentaly copy only those frames 
; in which ?middle-manager is supervisor of somebody
; and extend those frames with that somebody named ?x

; (?middle-manager = (Hacker Alyssa P)
;  ?person = (Bitdiddle Ben)
;  ?x = (Reasoner Louis))

; (?middle-manager = (Bitdiddle Ben)
;  ?person = (Warbucks Oliver)
;  ?x = (Hacker Alyssa P))

; (?middle-manager = (Bitdiddle Ben)
;  ?person = (Warbucks Oliver)
;  ?x = (Fect Cy D))

; (?middle-manager = (Bitdiddle Ben)
;  ?person = (Warbucks Oliver)
;  ?x = (Tweakit Lem E))

; (?middle-manager = (Scrooge Eben)
;  ?person = (Warbucks Oliver)
;  ?x = (Cratchet Robert))

; then ?person is unified with ?who and result is:

; ?who = (Bitdiddle Ben)
; ?who = (Warbucks Oliver)
; ?who = (Warbucks Oliver)
; ?who = (Warbucks Oliver)
; ?who = (Warbucks Oliver)

;;;;;;;;;;;;;;;;;
;;; Exercise 4.66
;;;;;;;;;;;;;;;;;

; What has Ben just realized?

; Ben realized that if he were to compute total sum of salaries of wheels
; then Oliver Warbucks salary would be added four times thus giving wrong sum

; Outline a method he can use to salvage the situation.

; just as Ben has
; (accumulation-function <variable> <query pattern>)

; he should also have
; (unique ?name <query pattern>)

; that filters stream of frames in such way that 
; no two frames contains ?name with equal values

; NOTE: (this was added day latter, previous inferior solution was removed)

; actually lisp-value is used as filter
; there is no problem to write (unique) function such that:

; (and (super-duper-query ?person-name)
;      (lisp-value (unique) ?person-name))

(define (unique)
  (let ((value-set '()))
    (lambda (x)
      (cond ((member x value-set) false)
	    (else (set! value-set (cons x value-set))
		  true)))))

(query-eval '(rule (better-wheel ?who)
		   (and (wheel ?who)
			(lisp-value (unique) ?who))))

(assert '(query-eval '(better-wheel ?who))
	'((better-wheel (warbucks oliver))
	  (better-wheel (bitdiddle ben))))

; actually for this thing to work properly I changed lisp-value function
; in query.scm so that it evaluates predicate function only once per query
; ofcourse this makes impossible to use variables in predicate like:

; (lisp-value (pred ?x) ?y ?z)

; but it can be worked around with lisp definition:

; (define (pred2 x y z)
;   ((pred x) y z))

; and using pred2 in lisp-value:

; (lisp-value pred2 ?x ?y ?z)

; what we gain is that we can have predicate with state

;;;;;;;;;;;;;;;;;
;;; Exercise 4.67
;;;;;;;;;;;;;;;;;

; in every frame that is created save reference to frame that it was created
; from and also save information about coresponding query that processed frame

; then every time we add new frame we can go back through chain of frames
; searching for somewhat identical frame that would signal a loop condition

;;;;;;;;;;;;;;;;;
;;; Exercise 4.68
;;;;;;;;;;;;;;;;;

(query-eval '(rule (append-to-form () ?y ?y)))

(query-eval '(rule (append-to-form (?u . ?v) ?y (?u . ?z))
		   (append-to-form ?v ?y ?z)))

(query-eval '(rule (reverse (?a . ?x) ?y)
		   (and (reverse ?x ?rev-x)
			(append-to-form ?rev-x (?a) ?y))))

(query-eval '(rule (reverse () ())))

; Can your rules answer both (reverse (1 2 3) ?x) and (reverse ?x (1 2 3)) ? 

; It answers (reverse (1 2 3) ?x) just fine.
; It also answers (reverse ?x (1 2 3)) but after that goes into infinite loop
; (I guess I have to try this once again after I make loop eliminator of 4.67)

(assert '(query-eval '(reverse (1 2 3) ?x))
	'((reverse (1 2 3) (3 2 1))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.69
;;;;;;;;;;;;;;;;;

(query-eval '(rule ((great . ?rel) ?x ?y)
		   (and (son ?x ?z)
			(?rel ?z ?y)
			(lisp-value pair? ?rel))))

; check for lisp pair is necessary to avoid:
; great grandson == great great son == great great great same

(query-eval '(rule ((great grandson) ?x ?y)
		   (and (son ?x ?z)
			(grandson ?z ?y))))

(assert '(query-eval '((great grandson) ?g ?ggs))
	'(((great grandson) mehujael jubal)
	  ((great grandson) irad lamech)
	  ((great grandson) mehujael jabal)
	  ((great grandson) enoch methushael)
	  ((great grandson) cain mehujael)
	  ((great grandson) adam irad)))

(assert '(query-eval '(?relationship Adam Irad))
	'(((great grandson) Adam Irad)))

(assert '(query-eval '(?relationship Adam Jabal))
	'(((great great great great great grandson) Adam Jabal)))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.70
;;;;;;;;;;;;;;;;;

; What is the purpose of the let bindings in the
; procedures add-assertion! and add-rule! ?

; purpose of let bindings in add-assertion! and add-rule! is to
; save value to old databases (THE-ASSERTIONS and THE-RULES),
; because stream-cons delays evaluation of second operand

; What would be wrong with the following implementation of add-assertion! ? 

; (define (add-assertion! assertion)
;   (store-assertion-in-index assertion)
;   (set! THE-ASSERTIONS
;         (cons-stream assertion THE-ASSERTIONS))
;   'ok)

; if THE-ASSERTIONS is stream (a1 a2 a3 ...) and we want to stream-cons
; a0 onto it then then implementation above would make THE-ASSERTIONS
; to be stream of (a0 a0 a0 ...) it is because evaluation of THE-ASSERTIONS
; as second operand of cons-stream is delayed and at the time it
; would be forced it's value would already be new THE-ASSERTIONS

;;;;;;;;;;;;;;;;;
;;; Exercise 4.71
;;;;;;;;;;;;;;;;;

; Can you give examples of queries where these simpler
; definitions would lead to undesirable behavior? 

; for recursive rules that has infinite number of answers 
; we can get situation were no answers are printed at all
; in simple-query first assertion will be processed at the
; same time as first rule (because it is not delayed)
; and if that rule is recursive then no matching
; assertions will be printed
 
(query-eval '(assert! (one 1)))
(query-eval '(assert! (ones (1))))
(query-eval '(rule (ones (?x . ?z))
		   (and (one ?x) 
			(ones ?z))))

; considering assertions above for query (ones ?x)
; old delayed functions would print endless stream of more and more "ones"
; while new functions without delay would print nothing

;;;;;;;;;;;;;;;;;
;;; Exercise 4.72
;;;;;;;;;;;;;;;;;

; Why do disjoin and stream-flatmap interleave 
; the streams rather than simply append them?

; because in cases with infinite answers interleaving varies
; answers from all tracing paths, it like Breadth-first search 
; (with interleaving) versus Depth-first search (without interleaving)

(query-eval '(assert! (two 2)))
(query-eval '(assert! (twos (2))))
(query-eval '(rule (twos (?x . ?z))
		   (and (two ?x) 
			(twos ?z))))

; with interleaving

;(or (ones (1)) (twos ?y))
;(or (ones ?x) (twos (2)))
;(or (ones (1 1)) (twos ?y))
;(or (ones ?x) (twos (2 2)))
;(or (ones (1 1 1)) (twos ?y))
;(or (ones ?x) (twos (2 2 2)))
;(or (ones (1 1 1 1)) (twos ?y))
;(or (ones ?x) (twos (2 2 2 2)))
;(or (ones (1 1 1 1 1)) (twos ?y))
;(or (ones ?x) (twos (2 2 2 2 2)))

; without interleaving

;(or (ones (1)) (twos ?y))
;(or (ones (1 1)) (twos ?y))
;(or (ones (1 1 1)) (twos ?y))
;(or (ones (1 1 1 1)) (twos ?y))
;(or (ones (1 1 1 1 1)) (twos ?y))
;(or (ones (1 1 1 1 1 1)) (twos ?y))
;(or (ones (1 1 1 1 1 1 1)) (twos ?y))
;(or (ones (1 1 1 1 1 1 1 1)) (twos ?y))
;(or (ones (1 1 1 1 1 1 1 1 1)) (twos ?y))
;(or (ones (1 1 1 1 1 1 1 1 1 1)) (twos ?y))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.73
;;;;;;;;;;;;;;;;;

; Why does flatten-stream use delay explicitly?

; What would be wrong with defining it as follows:

; (define (flatten-stream stream)
;   (if (stream-null? stream)
;       the-empty-stream
;       (interleave
;        (stream-car stream)
;        (flatten-stream (stream-cdr stream)))))

; interleave is common function, therefore for infinite stream
; flatten-stream would loop infinitely, because it calls itself
; (as second operand to interleave) without delaying that call
; and again result is that in case of infinite answers some won't be shown

; for example to print all sequences of "ones" that has even number of elements:

; (and (ones ?x) (lisp-value (lambda (x) (even? (length x))) ?x))

; does print answers with original flatten-stream, but does not with this one

;;;;;;;;;;;;;;;;;
;;; Exercise 4.74
;;;;;;;;;;;;;;;;;

; A.

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (stream-pair? x)
  (not (stream-null? x)))

(define (simple-flatten stream)
  (stream-map stream-car
              (stream-filter stream-pair? stream)))

; converts stream -> stream of streams where each stream in new stream is
; either empty or contails one element, so we first filter off empty streams
; and then take first element of those that are left - streams with one element

; B.

; No!
; because interleave-delayed is as good as append or stream-map 
; if streams to combine has one or none elements

;;;;;;;;;;;;;;;;;
;;; Exercise 4.75
;;;;;;;;;;;;;;;;;

(define (unique-query exps) (car exps))

(define (uniquely-asserted pattern frames)
  (stream-flatmap
   (lambda (frame)
     (let ((result (qeval (unique-query pattern) (singleton-stream frame))))
       (if (and (not (stream-null? result))
		(stream-null? (stream-cdr result)))
	   result
	   the-empty-stream)))
   frames))

(put 'unique 'qeval uniquely-asserted)

(assert '(query-eval '(and (supervisor ?inferior ?boss)
			   (unique (supervisor ?any-inferior ?boss))))
	'((and (supervisor (cratchet robert) (scrooge eben))
	       (unique (supervisor (cratchet robert) (scrooge eben))))
	  (and (supervisor (reasoner louis) (hacker alyssa p))
	       (unique (supervisor (reasoner louis) (hacker alyssa p))))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.76
;;;;;;;;;;;;;;;;;

; the way this exercise is defined it leads to nummerous errors:

; 1) "not" special form does not work properly:

; (and (salary ?x ?y)
;      (not (job ?x (computer programmer))))

; idea here is that salary generates frames with persons and their salaries
; then not special form filters off frames that were persons are programmers

; we can not eval both conjuncts seperatly on empty frame
; because that way "not" will lose it's chance to do the job

; 2) "lisp-value" special form does not work properly:

; (and (salary ?x ?y)
;      (lisp-value > ?y 50000))

; at the time of evaluating "lisp-value" ?y must be bound in all frames
; or else "lisp-value" will raise error condition, it is obvious that
; with new algorithm "lisp-value" would be evaluated "seperately"
; with only empty-frame and it will signal error because ?y is unbound

; 3) recursive rules does not work properly:

; consider exercise 4.63:
; (rule (son ?f ?s)
;       (and (wife ?f ?w)
;            (son ?w ?s))))

; if conjuncts are evaluated seperatelly then by applying this rule
; evaluator goes into infinite loop because evaluating (son ?x ?y) with
; some frame stream A leads to evaluation of (son ?w ?s) 
; with the same frame stream A, with both variables still unknown

(define (tagged-list-list? item seq)
  (if (null? seq)
      false
      (or (tagged-list? item (car seq))
	  (tagged-list-list? item (cdr seq)))))

; fix problems (1) and (2) by serializing "not" and "lisp-value" special forms

(define (crappy-conjoin conjuncts frame-stream)  
  (define (conjoin-by-streams result rest)
    (if (empty-conjunction? rest)
	result
	(conjoin-by-streams
	 (if (not (tagged-list-list? (first-conjunct rest) '(lisp-value not)))
	     (merge-streams result (qeval (first-conjunct rest) frame-stream))
	     (qeval (first-conjunct rest) result))
	 (rest-conjuncts rest))))
  (conjoin-by-streams frame-stream conjuncts))

(define  (merge-streams stream1 stream2)
  (stream-flatmap
   (lambda (frame1)
     (stream-filter
      (lambda (x) 
	(not (failure? x)))
      (stream-map
       (lambda (frame2)
	 (merge-frames frame1 frame2))
       stream2)))
   stream1))

(define (merge-frames frame1 frame2)
  (cond ((or (failure? frame1)
	     (failure? frame2))
	 'failed)
	((frame-empty? frame1) 
	 (merge-filters-into frame2 frame1))
	(else (merge-frames
	       (rest-bindings frame1)
	       (extend-if-possible
		(binding-variable (first-binding frame1))
		(binding-value (first-binding frame1))
		frame2)))))

(put 'crappy-and 'qeval crappy-conjoin)

; I think that problem (3) seriously invalidates this method

(assert '(query-eval '(crappy-and (supervisor ?x (Bitdiddle Ben))
				  (job ?x (computer programmer))))
	'((crappy-and (supervisor (fect cy d) (bitdiddle ben))
		      (job (fect cy d) (computer programmer)))
	  (crappy-and (supervisor (hacker alyssa p) (bitdiddle ben))
		      (job (hacker alyssa p) (computer programmer)))))

(assert '(query-eval '(crappy-and (salary ?x ?amount)
				  (lisp-value > ?amount 100000)))
	'((crappy-and (salary (warbucks oliver) 150000)
		      (lisp-value > 150000 100000))))

;;;;;;;;;;;;;;;;;;
;;; Loop detection
;;;;;;;;;;;;;;;;;;

; Loop detector was suggested by exercise 4.67, to quote:

; (After you study the details of the query-system
; implementation in section 4.4.4, you may want to
; modify the system to include your loop detector.)

(query-eval '(assert! (married Minnie Mickey)))

(query-eval '(rule (married ?x ?y)
		   (married ?y ?x)))

(assert '(query-eval '(married Mickey ?who))
	'((married Mickey Minnie)))

; this is Louis Reasoner looping outranked-by from exercise 4.64
(query-eval '(rule (lousy-outranked-by ?staff-person ?boss)
		   (or (supervisor ?staff-person ?boss)
		       (and (lousy-outranked-by ?middle-manager ?boss)
			    (supervisor ?staff-person ?middle-manager)))))

(assert '(query-eval '(lousy-outranked-by (Bitdiddle Ben) ?who))
	'((lousy-outranked-by (Bitdiddle Ben) (Warbucks Oliver))))

(query-eval '(rule (son ?f ?s)
		   (crappy-and (wife ?f ?w)
			       (son ?w ?s))))

; although it prints double amount of answers it seems
; that loop detection aleviates problem (3) of previous exercise
(assert '(query-eval '(son Lamech ?x))
	'((son lamech jubal)
	  (son lamech jubal)
	  (son lamech jabal)
	  (son lamech jabal)))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.77
;;;;;;;;;;;;;;;;;

; I guess this exercise is about fixing problems (1) and (2) of previous
; exercise 4.76, I think that I must read ahead before doing exercises

(define (seperate-conjoin conjuncts frame-stream)  
  (if (empty-conjunction? conjuncts)
      (singleton-stream (empty-frame))
      (merge-streams
       (qeval (first-conjunct conjuncts) frame-stream)
       (seperate-conjoin (rest-conjuncts conjuncts) frame-stream))))

(put 'seperate-and 'qeval seperate-conjoin)

(assert '(query-eval '(seperate-and (supervisor ?x (Bitdiddle Ben))
				    (job ?x (computer programmer))))
	'((seperate-and (supervisor (fect cy d) (bitdiddle ben))
			(job (fect cy d) (computer programmer)))
	  (seperate-and (supervisor (hacker alyssa p) (bitdiddle ben))
			(job (hacker alyssa p) (computer programmer)))))

(assert '(query-eval '(seperate-and (salary ?x ?amount)
				    (lisp-value > ?amount 100000)))
	'((seperate-and (salary (warbucks oliver) 150000)
			(lisp-value > 150000 100000))))

(assert '(query-eval '(seperate-and (lisp-value > ?amount 50000)
				    (salary ?x ?amount)))
	'((seperate-and (lisp-value > 75000 50000)
			(salary (scrooge eben) 75000))
	  (seperate-and (lisp-value > 150000 50000)
			(salary (warbucks oliver) 150000))
	  (seperate-and (lisp-value > 60000 50000)
			(salary (bitdiddle ben) 60000))))

(assert '(query-eval '(seperate-and (not (address ?x (Boston . ?y)))
				    (job ?x (computer programmer))))	
	'((seperate-and (not (address (fect cy d) (boston . ?y)))
			(job (fect cy d) (computer programmer)))
	  (seperate-and (not (address (hacker alyssa p) (boston . ?y)))
			(job (hacker alyssa p) (computer programmer)))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.78
;;;;;;;;;;;;;;;;;

(load "amb")

; some functions that does not depend on amb were implemented
; as primitives due to efficiency issues and the fact that
; amb-evaluator does not have "and" and "or" special forms

(define (pairs? a b)
  (and (pair? a) (pair? b)))

(define (starts-with-? x)
  (string=? (substring (symbol->string x) 0 1) "?"))

(define (var? x)
  (and (symbol? x) (starts-with-? x)))

(define (rename-var var number)
  (string->symbol
   (string-append (symbol->string var ) "-" (number->string number))))

(define rename-rule 
  (let ((number 0))
    (lambda (rule)
      (define (rename-rule-tree rule)
	(cond ((pair? rule)
	       (cons (rename-rule-tree (car rule)) 
		     (rename-rule-tree (cdr rule))))
	      ((var? rule) (rename-var rule number))
	      (else rule)))
      (set! number (+ number 1))
      (rename-rule-tree rule))))

(add-primitive 'rename-rule)

(define (follow-binding var frame)
  (let ((binding (assoc var frame)))
    (and binding
	 (let ((value (cdr binding)))
	   (if (var? value)
	       (or (follow-binding value frame) '?)
	       value)))))

(define (make-binding var frame)
  (cons var (follow-binding var frame)))

(define (purge-frame pattern frame)  
  (let ((vars nil))
    (define (walk-pattern x)
      (cond ((and (var? x) (not (memq x vars)))
	     (set! vars (cons x vars)))
	    ((pair? x)
	     (walk-pattern (car x))
	     (walk-pattern (cdr x)))
	    (else nil)))
    (walk-pattern pattern)
    (map (lambda (var) (make-binding var frame)) vars)))

(add-primitive 'purge-frame)

(define (display-list seq)
  (if (null? seq)
      'done
      (begin
	(debug (car seq) "\n")
	(display-list (cdr seq)))))

(add-primitive 'var?)
(add-primitive 'assoc)
(add-primitive 'equal?)
(add-primitive 'pairs?)

(eval-amb
 '(begin
    (define rules '())

    (define assertions '())

    (define empty-frame '())

    (define (require p)
      (if (not p) (amb)))

    (define (assert! x)
      (set! assertions (cons x assertions)))

    (define (rule conclusion body)
      (set! rules (cons (cons conclusion body) rules)))

    (define (conclusion rule)
      (car rule))

    (define (body rule)
      (cdr rule))

    (define (value binding) (cdr binding))

    (define (extend-frame var val frame)
      ; WARNING depends-on? not implemented
      (let ((binding (assoc var frame)))
	(if (not binding)
	    (cons (cons var val) frame)
	    (unify (value binding) val frame))))

    (define (unify a b frame)
      (cond ((equal? a b) frame)
	    ((var? a) (extend-frame a b frame))
	    ((var? b) (extend-frame b a frame))
	    ((pairs? a b)
	     (unify (car a)
		    (car b)
		    (unify (cdr a)
			   (cdr b)
			   frame)))
	    (else (amb))))

    (define (query-database pattern frame fn db)
      (define (query database)
	(require (not (null? database)))
	(amb (fn pattern (car database) frame)
	     (query (cdr database))))
      (query db))

    (define (query-assertions pattern frame)
      (query-database pattern frame unify assertions))

    (define (and-clauses clauses frame)
      (if (null? clauses)
	  frame
	  (and-clauses (cdr clauses) (eval (car clauses) frame))))

    (define (or-clauses clauses frame)
      (if (null? clauses)
	  (amb)
	  (amb (eval (car clauses) frame) (or-clauses (cdr clauses) frame))))

    (define (not-clauses clauses frame)
      (if (null? clauses)
	  frame
	  (let ((state 'not-done))
	    (let ((result (amb (eval (car clauses) frame) 'ok)))
	      (if (equal? state 'done)
		  (amb)
		  (begin
		    (permanent-set! state 'done)
		    (if (equal? result 'ok)
			frame
			(amb))))))))
    
    (define (query-logic pattern frame tag fn)
      (if (equal? tag (car pattern))
	  (fn (cdr pattern) frame)
	  (amb)))

    (define (try-rule pattern rule frame)
      (let ((new-rule (rename-rule rule)))
	(eval (body new-rule) (unify pattern (conclusion new-rule) frame))))

    (define (query-rules pattern frame)
      (query-database pattern frame try-rule rules))

    (define (eval pattern frame)
      (amb (query-logic pattern frame 'or or-clauses)
	   (query-logic pattern frame 'and and-clauses)	   
	   (query-logic pattern frame 'not not-clauses)	   
	   (query-assertions pattern frame)
	   (query-rules pattern frame)))

    (define (ask pattern)
      (purge-frame pattern (eval pattern empty-frame)))

    (assert! '(son Adam Cain))
    (assert! '(son Cain Enoch))
    (assert! '(son Enoch Irad))
    (assert! '(son Irad Mehujael))
    (assert! '(son Mehujael Methushael))
    (assert! '(son Methushael Lamech))
    (assert! '(woman Ada))
    (assert! '(wife Lamech Ada))
    (assert! '(son Ada Jabal))
    (assert! '(son Ada Jubal))

    (assert! '(true))

    (rule '(same ?x ?x) '(true))

    (rule '(grandson ?g ?s)
	  '(and (son ?f ?s)
		(son ?g ?f)))

    (rule '(son ?f ?s)
	  '(and (wife ?f ?w)
		(son ?w ?s)))))

(assert '(eval-amb-in-list
	  '(ask '(son ?parent ?son)))
	'(((?son . jubal) (?parent . ada))
	  ((?son . jabal) (?parent . ada))
	  ((?son . lamech) (?parent . methushael))
	  ((?son . methushael) (?parent . mehujael))
	  ((?son . mehujael) (?parent . irad))
	  ((?son . irad) (?parent . enoch))
	  ((?son . enoch) (?parent . cain))
	  ((?son . cain) (?parent . adam))
	  ((?son . jubal) (?parent . lamech))
	  ((?son . jabal) (?parent . lamech))))

(assert '(eval-amb-in-list 
	  '(ask '(and (wife ?father ?wife) (son ?wife ?son))))
	'(((?son . jubal) (?wife . ada) (?father . lamech))
	  ((?son . jabal) (?wife . ada) (?father . lamech))))

(debug "DON'T PANIC: this is may take some time!\n")

(assert '(eval-amb-in-list 
	  '(ask '(and (son ?father ?son) (son ?grandfather ?father))))
	'(((?grandfather . mehujael) (?son . lamech) (?father . methushael))
	  ((?grandfather . irad) (?son . methushael) (?father . mehujael))
	  ((?grandfather . enoch) (?son . mehujael) (?father . irad))
	  ((?grandfather . cain) (?son . irad) (?father . enoch))
	  ((?grandfather . adam) (?son . enoch) (?father . cain))
	  ((?grandfather . methushael) (?son . jubal) (?father . lamech))
	  ((?grandfather . methushael) (?son . jabal) (?father . lamech))))
	
; test variable renaming
(assert '(eval-amb-in-list 
	  '(ask '(grandson ?s enoch)))
	'(((?s . adam))))

(assert '(eval-amb-in-list
	  '(ask '(grandson Methushael ?grandson)))
	'(((?grandson . jubal))
	  ((?grandson . jabal))))

(assert '(eval-amb-in-list
	  '(ask '(or (son Adam ?son)
		     (son Lamech ?son))))
	'(((?son . cain))
	  ((?son . jubal))
	  ((?son . jabal))))

(assert '(eval-amb-in-list
	  '(ask '(and (or (son Adam ?son)
			  (son Lamech ?son))
		      (not (same ?son jabal)))))
	'(((?son . cain))
	  ((?son . jubal))))

(assert '(eval-amb-in-list
	  '(ask '(and (son ?parent ?son)
		      (not (woman ?parent)))))
	'(((?son . lamech) (?parent . methushael))
	  ((?son . methushael) (?parent . mehujael))
	  ((?son . mehujael) (?parent . irad))
	  ((?son . irad) (?parent . enoch))
	  ((?son . enoch) (?parent . cain))
	  ((?son . cain) (?parent . adam))
	  ((?son . jubal) (?parent . lamech))
	  ((?son . jabal) (?parent . lamech))))

;;;;;;;;;;;;;;;;;
;;; Exercise 4.79
;;;;;;;;;;;;;;;;;

; Implement for the query language a rule-application
; method that uses environments rather than renaming.

(define (unknown v f) v)

(define extend-name
  (let ((x 0))
    (lambda ()
      (set! x (+ x 1))
      (lambda (v f)
	(make-new-variable v x)))))

(define (apply-a-rule rule pattern query-frame)  
  (let ((clean-pattern (instantiate pattern query-frame (extend-name))))
    (simple-stream-flatmap
     (lambda (frame)
       (let* ((answer (instantiate clean-pattern frame unknown))
	      (result (unify-match answer pattern query-frame)))
	 (if (not (failure? result))
	     (singleton-stream result)
	     the-empty-stream)))
     (apply-a-actual-rule rule clean-pattern query-frame))))

(define (reset-frame frame)
  ; this is necessary to preserve stack for loop detector
  (list nil (cadr frame) (caddr frame)))

(define (apply-a-actual-rule rule pattern query-frame)
  (let* ((fresh-frame (reset-frame query-frame))
	 (unify-result (unify-match pattern (conclusion rule) fresh-frame)))
    (if (failure? unify-result)
	the-empty-stream
	(simple-stream-flatmap
	 (lambda (frame)
	   (let ((result (instantiate (conclusion rule) frame unknown)))
	     (let ((result-frame (unify-match pattern result query-frame)))
	       (if (not (failure? result-frame))
		   (singleton-stream result-frame)
		   the-empty-stream))))
	 (qeval (rule-body rule) (singleton-stream unify-result))))))

; to test how this code works copy/paste it at the end of query.scm
