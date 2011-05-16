;;;;EXPLICIT-CONTROL EVALUATOR FROM SECTION 5.4 OF
;;;; STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;;;Matches code in ch5.scm

;;; To use it
;;; -- load "load-eceval.scm", which loads this file and the
;;;    support it needs (including the register-machine simulator)

;;; -- To initialize and start the machine, do

;: (define the-global-environment (setup-environment))

;: (start eceval)

;; To restart, can do just
;: (start eceval)
;;;;;;;;;;

;;;SECTION 4.1.3
;;; operations used by compiled code and eceval except as noted

(define (true? x)
  (not (eq? x false)))

;;* not used by eceval itself -- used by compiled code when that
;; is run in the eceval machine
(define (false? x)
  (eq? x false))

(define (else? x)						; ex 5.24
  (and (pair? x) (eq? (car x) 'else)))				; ex 5.24

;;following compound-procedure operations not used by compiled code
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))
;;(end of compound procedures)


(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons 'ok (cons (make-frame vars vals) base-env))		; ex 5.30 b
      (if (< (length vars) (length vals))
	  '(too-many-arguments-supplied)			; ex 5.30 b
	  '(too-few-arguments-supplied))))			; ex 5.30 b


(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (cons 'ok (car vals)))				; ex 5.30 a
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
	; cons and get-variable-value is due to request:
	;   "return a distinguished condition code, which 
	;    cannot be a possible value of any user variable"
	; eg. I can assign '(unbound-variable-error) to variable "x" in
	; explicit-control evaluator and accessing "x" will not signal error
        '(unbound-variable-error)				; ex 5.30 a
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val)
	     '(ok))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        '(unbound-variable-error)				; ex 5.30 a
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (is-error? x)						; ex 5.30 a
  (and (pair? x) (not (eq? (car x) 'ok))))			; ex 5.30 a

(define (get-variable-value x)					; ex 5.30 a
  (cdr x))							; ex 5.30 a

(define (get-error-code x)					; ex 5.30 b
  (car x))							; ex 5.30 b

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


;;;SECTION 4.1.4

(define (setup-environment)
  (let ((initial-env
         (get-variable-value					; ex 5.30 b
	  (extend-environment (primitive-procedure-names)
			      (primitive-procedure-objects)
			      the-empty-environment))))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc))

; --- start ex 5.30 b ---
(define (check-list seq fn)  
  (if (null? seq)
      true
      (and (fn (car seq))
	   (check-list (cdr seq) fn))))

(define (primitive-safetynet p-fn args type-check)
  (lambda x 
    (cond ((and args (not (= (length x) args)))
	   '(incorrect-argument-count))
	  ((and type-check (not (check-list x type-check)))
	   '(incorrect-argument-type))
	  (else 
	   (cons 'ok (apply p-fn x))))))

(define (div/0-check div)
  (lambda x
    (cond ((null? x)
	   '(divide-without-arguments))
	  ; not (all are not zero) == some are zero
	  ((not (check-list (cdr x) (lambda (x) (not (zero? x)))))
	   '(divide-by-zero))
	  (else 
	   (apply div x)))))

(define primitive-procedures
  (list (list 'car (primitive-safetynet car 1 pair?))
        (list 'cdr (primitive-safetynet cdr 1 pair?))
        (list 'cons (primitive-safetynet cons 2 false))
        (list 'null? (primitive-safetynet null? 1 false))
	;;above from book -- here are some more
	(list '+ (primitive-safetynet + false number?))
	(list '- (primitive-safetynet - false number?))
	(list '* (primitive-safetynet * false number?))
	(list '= (primitive-safetynet = false number?))
	(list '/ (div/0-check (primitive-safetynet / false number?)))
	(list '> (primitive-safetynet > false number?))
	(list '< (primitive-safetynet < false number?))
	(list 'debug (primitive-safetynet debug false false)) ; I hope so
        ))
; --- end ex 5.30 b ---

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

;;; Simulation of new machine operations needed by
;;;  eceval machine (not used by compiled code)

;;; From section 5.4.1 footnote
(define (empty-arglist) '())
(define (adjoin-arg arg arglist)
  (append arglist (list arg)))
(define (last-operand? ops)
  (null? (cdr ops)))

;;; From section 5.4.2 footnote, for non-tail-recursive sequences
(define (no-more-exps? seq) (null? seq))

;;; From section 5.4.4 footnote
(define (get-global-environment)
  the-global-environment)
;; will do following when ready to run, not when load this file
(define the-global-environment (setup-environment))


;;; Simulation of new machine operations needed for compiled code
;;;  and eceval/compiler interface (not used by plain eceval machine)
;;; From section 5.5.2 footnote
(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))
(define (compiled-procedure? proc)
  (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))

;;**NB. To [not] monitor stack operations, comment in/[out] the line after
;; print-result in the machine controller below
;;**Also choose the desired make-stack version in regsim.scm

(define eceval-operations
  (list
   ;;primitive Scheme operations
   (list 'read read)

   ;;operations in syntax.scm
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'let? let?)						; ex 5.23
   (list 'let->lambda let->lambda)				; ex 5.23
   (list 'cond? cond?)						; ex 5.23
   (list 'cond->if cond->if)					; ex 5.23
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   (list 'is-error? is-error?)					; ex 5.30 a
   (list 'get-variable-value get-variable-value)		; ex 5.30 a
   (list 'get-error-code get-error-code)			; ex 5.30 b

   ;;operations in eceval-support.scm
   (list 'symbol? symbol?)					; ex 5.32
   (list 'true? true?)
   (list 'false? false?)					; ex 5.24
   (list 'else? else?)						; ex 5.24
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'prompt-for-input prompt-for-input)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	;for non-tail-recursive machine
   (list 'get-global-environment get-global-environment))
   )

(define eceval
  (make-machine
   '(exp env val proc argl continue unev)
   eceval-operations
  '(
;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; EC-Eval value:"))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

generic-error							; ex 5.30 a
  (assign val (op get-error-code) (reg val))			; ex 5.30 a
  (goto (label signal-error))					; ex 5.30 a

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;SECTION 5.4.1
eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  ; exercises 5.23 and 5.24 are not compatible
  ; comment out next two lines to test exercise 5.23
  (test (op cond?) (reg exp))					; ex 5.24
  (branch (label ev-bare-cond)) 				; ex 5.24

  (test (op let?) (reg exp))					; ex 5.23
  (branch (label ev-let))					; ex 5.23
  (test (op cond?) (reg exp))					; ex 5.23
  (branch (label ev-cond))					; ex 5.23
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-cond 							; ex 5.23
  (assign exp (op cond->if) (reg exp))				; ex 5.23  
  (goto (label eval-dispatch))					; ex 5.23

ev-let								; ex 5.23
  (assign exp (op let->lambda) (reg exp))			; ex 5.23
  (goto (label eval-dispatch))					; ex 5.23

; --- start ex 5.24 ---
ev-bare-cond
  (save continue)
  (assign unev (reg exp))
next-cond-clause
  (assign unev (op rest-exps) (reg unev))			
  (assign val (const *cond-failure*))
  (test (op no-more-exps?) (reg unev))
  (branch (label just-return-val))
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))	
  (branch (label cond-last-operand))
test-cond-as-usual
  (save exp)
  (assign exp (op first-exp) (reg exp))
  (save env)
  (save unev)
  (assign continue (label cond-test))
  (goto (label eval-dispatch))
cond-test
  (restore unev)
  (restore env)
  (restore exp)
  (test (op false?) (reg val))
  (branch (label next-cond-clause))
eval-cond-body
  (assign unev (op rest-exps) (reg exp))
  (test (op no-more-exps?) (reg unev))
  (branch (label just-return-val))
  (goto (label ev-sequence))
just-return-val
  (restore continue)
  (goto (reg continue))
cond-last-operand
  (test (op else?) (reg exp))
  (branch (label eval-cond-body))  
  (goto (label test-cond-as-usual))
; --- end ex 5.24 ---

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (test (op is-error?) (reg val))				; ex 5.30 a
  (branch (label generic-error))				; ex 5.30 a
  (assign val (op get-variable-value) (reg val))		; ex 5.30 a
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

look-up-symbol-proc						; ex 5.32
  (assign val (op lookup-variable-value) (reg exp) (reg env))	; ex 5.32
  ; next three instructions is due to ex 5.30 error handling	; ex 5.32
  (test (op is-error?) (reg val))				; ex 5.32
  (branch (label generic-error))				; ex 5.32
  (assign val (op get-variable-value) (reg val))		; ex 5.32
  (goto (label ev-symbol-proc))					; ex 5.32

ev-application
  (save continue)
  (assign unev (op operands) (reg exp))
  (assign exp (op operator) (reg exp))
  (test (op symbol?) (reg exp))					; ex 5.32
  (branch (label look-up-symbol-proc))				; ex 5.32
  (save env)
  (save unev)
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
ev-symbol-proc							; ex 5.32
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
  (goto (label unknown-procedure-type))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (test (op is-error?) (reg val))				; ex 5.30 b
  (branch (label generic-error))				; ex 5.30 b
  (assign val (op get-variable-value) (reg val))		; ex 5.30 b
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign val (op extend-environment)
              (reg unev) (reg argl) (reg env))  
  (test (op is-error?) (reg val))				; ex 5.30 b
  (branch (label generic-error))				; ex 5.30 b
  (assign env (op get-variable-value) (reg val))		; ex 5.30 b
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

;;;SECTION 5.4.2
ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

; comment out this block to disable tail recursion (ex 5.28)
#|
ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-end
  (restore continue)
  (goto (reg continue))
|#

; comment in this block to disable tail recursion (ex 5.28)
ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;;SECTION 5.4.3

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (assign val (op set-variable-value!) (reg unev) (reg val) (reg env))
  (test (op is-error?) (reg val))				; ex 5.30 a
  (branch (label generic-error))				; ex 5.30 a
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
   )))

'ok
