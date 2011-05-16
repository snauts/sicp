(define (c-compile exp db linkage)
  (cond ((c-self-evaluating? exp)
	 (c-compile-self-evaluating exp linkage))
	((quoted? exp)
	 (c-compile-quoted exp db linkage))
        ((assignment? exp)
         (c-compile-assignment exp db linkage))
        ((definition? exp)
         (c-compile-definition exp db linkage))
        ((variable? exp)
         (c-compile-variable exp db linkage))
        ((begin? exp)
         (c-compile-sequence (begin-actions exp) db linkage))
        ((lambda? exp)
	 (c-compile-lambda exp db linkage))
        ((cond? exp)
	 (c-compile (cond->if exp) db linkage))
        ((let? exp)
	 (c-compile (let->combination exp) db linkage))
        ((if? exp)
	 (c-compile-if exp db linkage))
        ((application? exp)
	 (c-compile-application exp db linkage))
        (else (error "Unknown expression type -- COMPILE" exp))))

(define (c-finish var linkage)
  (cond ((tagged-list? linkage 'return)
	 `("return " ,@var ";"))
	((tagged-list? linkage 'execute)
	 `(,@var ,@(cdr linkage)))
	((tagged-list? linkage 'assign-to)
	 `(,(second linkage) " = " ,@var ";"))
	((tagged-list? linkage 'init-to)
	 `("unsigned " ,(second linkage) " = " ,@var ";"))
	(else (error "C-FINISH unknown linkage"))))

(define (c-self-evaluating? exp)
  (cond ((number? exp) true)
        (else false)))

(define (make-finish code linkage)
  (list (c-finish code linkage)))

(define (c-compile-self-evaluating exp linkage)
  (make-finish `("lisp_number(" ,exp ")") linkage))

(define (c-format-line seq)
  (if (null? seq)
      'done
      (begin
	(debug (car seq))
	(c-format-line (cdr seq)))))

(define (c-format-code seq)
  (if (null? seq)
      (debug "\n")
      (begin
	(debug "\n")
	(c-format-line (car seq))
	(c-format-code (cdr seq)))))

(define empty-line `((" ")))

(define (db-declaration db)
  (caddr db))

(define (append-declaration declaration db)
  (set-car! (cddr db) (append declaration (db-declaration db))))

(define (db-code db)
  (cadr db))

(define (append-procedure code db)
  (set-car! (cdr db) (append code (db-code db))))

(define (db-symbols db)
  (car db))

(define (get-symbol name db)
  (let ((lookup (assoc name (car db))))
    (if lookup
	(cdr lookup)
	(let ((sym (make-label 'sym)))
	  (set-car! db (cons (cons name sym) (car db)))
	  sym))))

(define (c-compile-variable exp db linkage)
  (let ((variable (get-symbol exp db)))
    (make-finish `("lookup_variable(" ,variable ", env)") linkage)))
 
(define (combine-code . codes)
  (apply append codes))

(define (c-compile-def/assign fn exp db linkage get-variable get-value)
  (let* ((val (make-label 'val))	 
	 (var (get-variable exp))
	 (value-exp (get-value exp))
	 (sym (get-symbol var db)))
    (combine-code 
     `(("unsigned " ,val ";")) 
     (c-compile value-exp db `(assign-to ,val))
     (list (list fn sym ", " val ", env);")
	   (c-finish '("done()") linkage)))))

(define (c-compile-assignment exp db linkage)
  (c-compile-def/assign "set_variable("
			exp db linkage
			assignment-variable
			assignment-value))

(define (c-compile-definition exp db linkage)
  (c-compile-def/assign "define_variable("
			exp db linkage
			definition-variable
			definition-value))

(define (c-compile-symbol exp db linkage)
  (let ((sym (get-symbol exp db)))
    (make-finish (list sym) linkage)))

(define (c-compile-cons exp db linkage)
  (let* ((a (c-compile-quoted `(quote ,(car exp)) db '(execute)))
	 (b (c-compile-quoted `(quote ,(cdr exp)) db '(execute))))
    (list (c-finish `("cons(" ,@(last a) ", " ,@(last b) ")") linkage))))

(define (c-compile-quoted exp db linkage)
  (let ((text (text-of-quotation exp)))
    (cond ((c-self-evaluating? text)
	   (c-compile-self-evaluating text db linkage))
	  ((symbol? text)
	   (c-compile-symbol text db linkage))
	  ((null? text)
	   (make-finish (list "NIL") linkage))
	  ((pair? text)
	   (c-compile-cons text db linkage))
	  (else (error "unknown quotation")))))

(define (c-compile-sequence exp db linkage)
  (if (last-exp? exp)
      (c-compile (first-exp exp) db linkage)
      (combine-code
       (c-compile (first-exp exp) db '(execute ";"))
       (c-compile-sequence (rest-exps exp) db linkage))))

(define (indent-code code)
  (map (lambda (line) (append '("    ") line)) code))

(define (c-compile-if exp db linkage)
  (let ((val (make-label 'val)))
    (combine-code
     (c-compile (if-predicate exp) db `(init-to ,val))
     `(("if (" ,val " != false) {"))
     (indent-code (c-compile (if-consequent exp) db linkage))
     `(("}"))
     `(("else {"))
     (indent-code (c-compile (if-alternative exp) db linkage))
     `(("}")))))

(define (c-compile-arg-list db args argl tmp)
  (if (null? args)
      '()
      (combine-code
       (c-compile (car args) db `(assign-to ,tmp))
       `((,argl " = cons(" ,tmp ", " ,argl ");"))
       (c-compile-arg-list db (cdr args) argl tmp))))

(define (c-compile-maybe-args exp db argl)
  (if (null? (operands exp))
      '()
      (let ((tmp (make-label 'tmp)))
	(combine-code
	 `(("unsigned " ,tmp ";"))
	 (c-compile-arg-list db (reverse (operands exp)) argl tmp)))))

(define (c-compile-application exp db linkage)
  (let ((argl (make-label 'argl))
	(proc (make-label 'proc)))
    (combine-code
     `(("unsigned " ,argl " = NIL;"))
     (c-compile-maybe-args exp db argl)
     (c-compile (operator exp) db `(init-to ,proc))
     (make-finish `("apply_fn(" ,proc ", " ,argl ")") linkage))))
     
(define (c-compile-body exp db linkage)
  (let ((fn (make-label 'fn)))
    (append-declaration `(("static unsigned " ,fn "(unsigned env);")) db)
    (append-procedure
     (combine-code
      `(("static unsigned " ,fn "(unsigned env) {"))
      (indent-code (c-compile-sequence (lambda-body exp) db '(return)))
      `(("}"))
      empty-line)
     db)
    fn))

(define (c-compile-parameters exp db)
  (last (c-compile `(quote ,(lambda-parameters exp)) db '(execute))))

(define (c-compile-proc sym parms fn)
  `("install_compiled(" ,sym ", env, " ,parms ", " ,fn ")"))

(define (c-compile-lambda exp db linkage)
  (let* ((fn (c-compile-body exp db linkage))
	 (proc (make-label 'proc))
	 (parms (make-label 'parms))
	 (sym (get-symbol fn db)))
    (combine-code
     `(("unsigned " ,parms " = " ,@(c-compile-parameters exp db) ";"))
     `(("static unsigned " ,proc " = NIL;"))
     `(("if (" ,proc " == NIL) {"))
     (indent-code `((, proc " = install_primitive(" ,sym ", &" ,fn ");")))
     `(("}"))
     (make-finish (c-compile-proc sym parms proc) linkage))))

(define (compile-single-symbol x)
  `(,(cdr x) " = get_symbol(\"" ,(car x) "\");"))

(define (compile-symbols syms)
  (combine-code
   (map (lambda (x) `("static unsigned " ,(cdr x) ";")) syms)
   empty-line
   `(("static void init_lisp_symbols(void) {"))
   (indent-code (map compile-single-symbol syms))
   `(("}"))
   empty-line))

(define (c-compile-top-level code)
  (let* ((db (list nil nil nil))
	 (main (c-compile code db '(return))))
    (combine-code
     (compile-symbols (db-symbols db))
     (db-declaration db)
     (db-code db)
     `(("static unsigned lisp_main(unsigned env) {"))
     (indent-code main)
     `(("}")))))

(define (compile-to-file code)
  (with-output-to-file "scheme.h"
    (lambda () (c-format-code (c-compile-top-level code)))))

(define (let? exp) (tagged-list? exp 'let))

(define (let->combination exp)
  (cons (cons 'lambda 
	      (cons (map car (cadr exp)) 
		    (cddr exp)))
	(map cadr (cadr exp))))

;; ------ compiler ends here, code of evaluator follows -----------------------

(define program
  '(begin
     (define (list . x) 
       x)

     (define (cadr x)
       (car (cdr x)))

     (define (cdadr x)
       (cdr (car (cdr x))))

     (define (cadddr x)
       (car (cdr (cdr (cdr x)))))

     (define (caadr x)
       (car (car (cdr x))))

     (define (cddr x)
       (cdr (cdr x)))

     (define (caddr x)
       (car (cdr (cdr x))))

     (define (cdddr x)
       (cdr (cdr (cdr x))))

     (define (map proc seq)
       (if (null? seq)
	   seq
	   (cons (proc (car seq)) 
		 (map proc (cdr seq)))))

     (define (length seq)
       (if (null? seq)
	   0
	   (+ 1 (length (cdr seq)))))

     (define (error msg)
       (display msg)
       (quit))

     (define (eval exp env)
       (cond ((self-evaluating? exp) exp)
	     ((variable? exp) (lookup-variable-value exp env))
	     ((quoted? exp) (text-of-quotation exp))
	     ((assignment? exp) (eval-assignment exp env))
	     ((definition? exp) (eval-definition exp env))
	     ((let? exp) (eval (let->combination exp) env))
	     ((if? exp) (eval-if exp env))
	     ((lambda? exp)
	      (make-procedure (lambda-parameters exp)
			      (lambda-body exp)
			      env))
	     ((begin? exp) 
	      (eval-sequence (begin-actions exp) env))
	     ((cond? exp) (eval (cond->if exp) env))
	     ((application? exp)
	      (weak-apply (eval (operator exp) env)
			  (list-of-values (operands exp) env)))
	     (else
	      (error '(unknown expression type)))))

     (define (weak-apply procedure arguments)
       (cond ((primitive-procedure? procedure)
	      (apply-primitive-procedure procedure arguments))
	     ((compound-procedure? procedure)
	      (eval-sequence
	       (procedure-body procedure)
	       (extend-environment
		(procedure-parameters procedure)
		arguments
		(procedure-environment procedure))))
	     (else
	      (error '(unknown procedure type)))))

     (define (list-of-values exps env)
       (if (no-operands? exps)
	   '()
	   (cons (eval (first-operand exps) env)
		 (list-of-values (rest-operands exps) env))))

     (define (eval-if exp env)
       (if (true? (eval (if-predicate exp) env))
	   (eval (if-consequent exp) env)
	   (eval (if-alternative exp) env)))

     (define (eval-sequence exps env)
       (cond ((last-exp? exps) (eval (first-exp exps) env))
	     (else (eval (first-exp exps) env)
		   (eval-sequence (rest-exps exps) env))))

     (define (eval-assignment exp env)
       (set-variable-value! (assignment-variable exp)
			    (eval (assignment-value exp) env)
			    env)
       'ok)

     (define (eval-definition exp env)
       (define-variable! (definition-variable exp)
	 (eval (definition-value exp) env)
	 env)
       'ok)

     (define (self-evaluating? exp)
       (cond ((number? exp) true)
	     (else false)))

     (define (variable? exp) (symbol? exp))

     (define (quoted? exp)
       (tagged-list? exp 'quote))

     (define (text-of-quotation exp) (cadr exp))

     (define (tagged-list? exp tag)
       (if (pair? exp)
	   (eq? (car exp) tag)
	   false))

     (define (assignment? exp)
       (tagged-list? exp 'set!))
     (define (assignment-variable exp) (cadr exp))
     (define (assignment-value exp) (caddr exp))

     (define (definition? exp)
       (tagged-list? exp 'define))
     (define (definition-variable exp)
       (if (symbol? (cadr exp))
	   (cadr exp)
	   (caadr exp)))
     (define (definition-value exp)
       (if (symbol? (cadr exp))
	   (caddr exp)
	   (make-lambda (cdadr exp)   ; formal parameters
			(cddr exp)))) ; body

     (define (lambda? exp) (tagged-list? exp 'lambda))
     (define (lambda-parameters exp) (cadr exp))
     (define (lambda-body exp) (cddr exp))

     (define (make-lambda parameters body)
       (cons 'lambda (cons parameters body)))

     (define (if? exp) (tagged-list? exp 'if))
     (define (if-predicate exp) (cadr exp))
     (define (if-consequent exp) (caddr exp))
     (define (if-alternative exp)
       (if (not (null? (cdddr exp)))
	   (cadddr exp)
	   'false))

     (define (make-if predicate consequent alternative)
       (list 'if predicate consequent alternative))

     (define (let? exp) (tagged-list? exp 'let))
     (define (let->combination exp)
       (cons (cons 'lambda 
		   (cons (map car (cadr exp)) 
			 (cddr exp)))
	     (map cadr (cadr exp))))

     (define (begin? exp) (tagged-list? exp 'begin))
     (define (begin-actions exp) (cdr exp))
     (define (last-exp? seq) (null? (cdr seq)))
     (define (first-exp seq) (car seq))
     (define (rest-exps seq) (cdr seq))

     (define (sequence->exp seq)
       (cond ((null? seq) seq)
	     ((last-exp? seq) (first-exp seq))
	     (else (make-begin seq))))
     (define (make-begin seq) (cons 'begin seq))

     (define (application? exp) (pair? exp))
     (define (operator exp) (car exp))
     (define (operands exp) (cdr exp))
     (define (no-operands? ops) (null? ops))
     (define (first-operand ops) (car ops))
     (define (rest-operands ops) (cdr ops))

     (define (cond? exp) (tagged-list? exp 'cond))
     (define (cond-clauses exp) (cdr exp))
     (define (cond-else-clause? clause)
       (eq? (cond-predicate clause) 'else))
     (define (cond-predicate clause) (car clause))
     (define (cond-actions clause) (cdr clause))
     (define (cond->if exp)
       (expand-clauses (cond-clauses exp)))

     (define (cond-else first rest)
       (if (null? rest)
	   (sequence->exp (cond-actions first))
	   (error '(ELSE clause isn't last -- COND->IF))))

     (define (cond-clause first rest)
       (if (null? (cond-actions first))
	   (list (list 'lambda '(val)
		       (list 'if 'val 'val (expand-clauses rest)))
		 (cond-predicate first))
	   (make-if (cond-predicate first)
		    (sequence->exp (cond-actions first))
		    (expand-clauses rest))))

     (define (expand-clauses clauses)
       (if (null? clauses)
	   'false                          ; no else clause
	   (let ((first (car clauses))
		 (rest (cdr clauses)))
	     (if (cond-else-clause? first)
		 (cond-else first rest)
		 (cond-clause first rest)))))

     (define (make-procedure parameters body env)
       (list 'procedure parameters body env))
     (define (compound-procedure? p)
       (tagged-list? p 'procedure))
     (define (procedure-parameters p) (cadr p))
     (define (procedure-body p) (caddr p))
     (define (procedure-environment p) (cadddr p))

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
	   (cons (make-frame vars vals) base-env)
	   (if (< (length vars) (length vals))
	       (error '(Too many arguments supplied))
	       (error '(Too few arguments supplied)))))

     (define (lookup-variable-value var env)
       (define (env-loop env)
	 (define (scan vars vals)
	   (cond ((null? vars)
		  (env-loop (enclosing-environment env)))
		 ((eq? var (car vars))
		  (car vals))
		 (else (scan (cdr vars) (cdr vals)))))
	 (if (eq? env the-empty-environment)
	     (error (list 'Unbound 'variable var))
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
		  (set-car! vals val))
		 (else (scan (cdr vars) (cdr vals)))))
	 (if (eq? env the-empty-environment)
	     (error '(Unbound variable))
	     (let ((frame (first-frame env)))
	       (scan (frame-variables frame)
		     (frame-values frame)))))
       (env-loop env))

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

     (define primitive-procedures
       (list (list '+ +)
	     (list '- -)
	     (list '= =)
	     (list '* *)
	     (list '/ /)
	     (list '> >)
	     (list '< <)
	     (list 'quit quit)
	     (list 'car car)
	     (list 'cdr cdr)
	     (list 'cons cons)
	     (list 'glue cons)
	     (list 'first car)
	     (list 'rest cdr)
	     (list 'list list)
	     (list 'even? even?)
	     (list 'null? null?)))

     (define (primitive-procedure-names)
       (map car
	    primitive-procedures))

     (define (primitive-procedure-objects)
       (map (lambda (proc) (list 'primitive (cadr proc)))
	    primitive-procedures))

     (define (setup-environment)
       (let ((initial-env
	      (extend-environment (primitive-procedure-names)
				  (primitive-procedure-objects)
				  the-empty-environment)))
	 (define-variable! 'true true initial-env)
	 (define-variable! 'false false initial-env)
	 initial-env))

     (define the-global-environment (setup-environment))

     (define (primitive-procedure? proc)
       (tagged-list? proc 'primitive))

     (define (primitive-implementation proc) (cadr proc))

     (define (apply-primitive-procedure proc args)
       (apply (primitive-implementation proc) args))

     (define input-prompt '(M-Eval input))
     (define output-prompt '(M-Eval value))

     (define (driver-loop)
       (prompt-for-input input-prompt)
       (let ((input (read)))
	 (let ((output (eval input the-global-environment)))
	   (announce-output output-prompt)
	   (user-print output)))
       (driver-loop))
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
     
     ;(driver-loop)
     ))

; (debug "\n")

; (c-format-code (c-compile-top-level program))

(compile-to-file program)

'(C-COMPILER LOADED)
