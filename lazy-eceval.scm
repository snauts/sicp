(define (delay-it exp env)
  (list 'thunk exp env))

(define (thunk? obj)
  (tagged-list? obj 'thunk))

(define (thunk-exp thunk) (cadr thunk))

(define (thunk-env thunk) (caddr thunk))

(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (update-thunk! obj result)
  (set-car! obj 'evaluated-thunk)
  (set-car! (cdr obj) result)
  (set-cdr! (cdr obj) '()))

(define lazy-eceval-operations
  (append eceval-operations
	  (list (list 'delay-it delay-it)
		(list 'debug debug)
		(list 'thunk? thunk?)
		(list 'thunk-exp thunk-exp)
		(list 'thunk-env thunk-env)
		(list 'thunk-value thunk-value)
		(list 'update-thunk! update-thunk!)
		(list 'evaluated-thunk? evaluated-thunk?))))

(define lazy-eceval
  (make-machine
   '(exp env val proc argl continue unev)
   lazy-eceval-operations
  '(
;;SECTION 5.4.4
read-eval-print-loop
  (perform (op initialize-stack))
  (perform
   (op prompt-for-input) (const ";;; Lazy EC-Eval input:"))
  (assign exp (op read))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label ev-actual-value))				; ex 5.25
print-result
;;**following instruction optional -- if use it, need monitored stack
  (perform (op print-stack-statistics))
  (perform
   (op announce-output) (const ";;; Lazy EC-Eval value:"))
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
  (test (op let?) (reg exp))					; ex 5.23
  (branch (label ev-let))					; ex 5.23
  (test (op cond?) (reg exp))					; ex 5.23
  (branch (label ev-cond))					; ex 5.23
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-cond								; ex 5.23
  (assign exp (op cond->if) (reg exp))				; ex 5.23  
  (goto (label eval-dispatch))					; ex 5.23

ev-let								; ex 5.23
  (assign exp (op let->lambda) (reg exp))			; ex 5.23
  (goto (label eval-dispatch))					; ex 5.23

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

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label ev-actual-value))				; ex 5.25
ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (test (op compound-procedure?) (reg proc))			; ex 5.25
  (branch (label ev-delay-operand-loop))			; ex 5.25
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label ev-actual-value))				; ex 5.25
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label ev-actual-value))				; ex 5.25
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

; --- start ex 5.25 ---
ev-delay-operand-loop
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (assign exp (op first-operand) (reg unev))
  (assign val (op delay-it) (reg exp) (reg env))
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-delay-operand-loop))
; --- end ex 5.25 ---

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
  (goto (label ev-actual-value))				; ex 5.25
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

; --- everything below is for ex 5.25

;(define (actual-value exp env)
;  (force-it (eval exp env)))

ev-actual-value
  (save continue)
  (assign continue (label ev-actual-value-eval-done))
  (goto (label eval-dispatch))
ev-actual-value-eval-done
  (restore continue)
  (goto (label ev-force-it)) ; pass val from eval "as is" to force-it

;(define (force-it obj)
;  (cond ((thunk? obj)
;         (let ((result (actual-value
;                        (thunk-exp obj)
;                        (thunk-env obj))))
;           (set-car! obj 'evaluated-thunk)
;           (set-car! (cdr obj) result)  ; replace exp with its value
;           (set-cdr! (cdr obj) '())     ; forget unneeded env
;           result))
;        ((evaluated-thunk? obj)
;         (thunk-value obj))
;        (else obj)))

ev-force-it ; force-it argument "obj" is in val
  (test (op thunk?) (reg val))
  (branch (label ev-just-thunk))
  (test (op evaluated-thunk?) (reg val))
  (branch (label ev-evaluated-thunk))
  (goto (reg continue)) ; obj already in val
ev-evaluated-thunk
  (assign val (op thunk-value) (reg val))
  (goto (reg continue))
ev-just-thunk
  (assign exp (op thunk-exp) (reg val))
  (assign env (op thunk-env) (reg val))
  (save continue)
  (save val) ; save obj
  (assign continue (label ev-force-it-actual-value-done))
  (goto (label ev-actual-value))
ev-force-it-actual-value-done ; now val is result
  (restore exp) ; now obj in exp
  (perform (op update-thunk!) (reg exp) (reg val))
  (restore continue)
  (goto (reg continue))

   )))

'ok

