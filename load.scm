 ; only for PLT scheme

(define nil '())

(define pi 3.14159265)

(define (debug . rest) (map display rest) (display ""))

(define (r) (restart 1))

(define (do-test value result)
  (or (and (procedure? value) 
	   (value result))
      (equal? value result)))

(define (test-message value)
  (if (procedure? value)
      "something else"
      value))

(if (not (environment-bound? system-global-environment 'builtin-eval))
    (eval '(define builtin-eval eval) system-global-environment))

(define (assert expr value) ; stole this from Einars
  (let ((result (if (not (procedure? expr))
		    (builtin-eval expr user-initial-environment)
		    (expr))))		    
    (if (do-test value result)
	(debug "PASS ")
	(debug "FAIL "))
    (debug expr "\n   = " result "\n")
    (if (do-test value result)
	true
	(debug "     Expected " (test-message value) "\n"))))

(define (dbg x)
  (debug x "\n")
  x)

(define (sleep seconds)
  (sleep-current-thread (* 1000 seconds)))

(define original-assoc assoc)
(define original-append append)

(load "chapter1")
(load "chapter2")
(load "chapter3")

(set! assoc original-assoc)

(load "chapter4")

(set! append original-append)

(load "chapter5")
