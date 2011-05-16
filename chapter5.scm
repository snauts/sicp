;;;;;;;;;;;;;;;;;
;;; Exercise 5.1
;;;;;;;;;;;;;;;;;

; picture ex-5.1.png

;;;;;;;;;;;;;;;;;
;;; Exercise 5.2
;;;;;;;;;;;;;;;;;

;(controller
;   (assign product (const 1))				;    li     v0, 1
;   (assign counter (const 1))				;    li     t0, 1
; test-counter						; 1:
;   (test (op >) (reg counter) (reg n))			;    subu   t1, t0, a0
;   (branch (label factorial-done))			;    bgtz   t1, 2f
;   (assign product (op *) (reg counter) (reg product))	;    mul    v0, t0, v0
;   (assign counter (op +) (reg counter) (const 1))	;    addi   t0, t0, 1
;   (goto (label test-counter))				;    b      1b
; factorial-done)					; 2:

; MIPS assembly language for comparison

(load "regsim")

(define factorial-machine
  (make-machine
   '(n product counter)
    (list (list '* *) (list '+ +) (list '> >))
   '((assign product (const 1))
     (assign counter (const 1))
   test-counter
     (test (op >) (reg counter) (reg n))
     (branch (label factorial-done))
     (assign product (op *) (reg counter) (reg product))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-counter))
   factorial-done)))

(set-register-contents! factorial-machine 'n 6)

(start factorial-machine)

(assert '(get-register-contents factorial-machine 'product) 720)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.3
;;;;;;;;;;;;;;;;;

(define sqrt-machine
  (make-machine
   '(x guess tmp)
    (list (list '/ /) (list '* *) (list '+ +)  (list '- -) (list '> >))
   '((assign guess (const 1.0))
   next-guess
     (assign tmp (op *) (reg guess) (reg guess))
     (assign tmp (op -) (reg tmp) (reg x))
     (test (op >) (reg tmp) (const 0.0))
     (branch (label do-not-invert))
     (assign tmp (op -) (const 0.0) (reg tmp))
   do-not-invert
     (test (op >) (const 0.0001) (reg tmp))
     (branch (label sqrt-done))
     (assign tmp (op /) (reg x) (reg guess))
     (assign tmp (op +) (reg tmp) (reg guess))
     (assign guess (op *) (const 0.5) (reg tmp))
     (goto (label next-guess))
   sqrt-done)))

(set-register-contents! sqrt-machine 'x 2.0)

(start sqrt-machine)

(assert '(get-register-contents sqrt-machine 'guess) 
	(lambda (x) (< 1.4141 x 1.4143)))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.4
;;;;;;;;;;;;;;;;;

; both data path diagrams are in ex-5.4.png

; A.

(define expt-machine
  (make-machine
   '(b n result continue)
    (list (list '* *) (list '- -) (list '= =))
   '((assign continue (label expt-done))     
   expt
     (test (op =) (reg n) (const 0))
     (branch (label expt-terminal))
     (assign n (op -) (reg n) (const 1))
     (save continue)
     (assign continue (label after-expt))
     (goto (label expt))
   after-expt
     (restore continue)
     (assign result (op *) (reg b) (reg result))
     (goto (reg continue))
   expt-terminal
     (assign result (const 1))
     (goto (reg continue))
   expt-done)))

(set-register-contents! expt-machine 'b 3)
(set-register-contents! expt-machine 'n 4)

(start expt-machine)

(assert '(get-register-contents expt-machine 'result) 81)

; B.

(define expt-machine
  (make-machine
   '(b n counter product)
    (list (list '* *) (list '- -) (list '= =))
   '((assign counter (reg n))
     (assign product (const 1))
   expt     
     (test (op =) (reg counter) (const 0))
     (branch (label expt-done))
     (assign counter (op -) (reg counter) (const 1))
     (assign product (op *) (reg b) (reg product))
     (goto (label expt))
   expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 8)

(start expt-machine)

(assert '(get-register-contents expt-machine 'product) 256)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.5
;;;;;;;;;;;;;;;;;

;;; factorial ;;;

;  stack:
;   call: (factorial 3)
;  stack: fact-done | 3
;   call:   (factorial 2)
;  stack:   fact-done | 3 | after-fact | 2
;   call:     (factorial 1)
; return:     (factorial 1)
; return:   (factorial 2)
;  stack: fact-done | 3
; return: (factorial 3)
;  stack:

;;; fibonacci ;;;

;  stack:
;   call: (fib 3)
;  stack: fib-done | 3
;   call:   (fib 2)
;  stack:   fib-done | 3 | afterfib-n-1 | 2
;   call:     (fib 1)
; return:     (fib 1)
;  stack:   fib-done | 3 | afterfib-n-1 | 1
;   call:     (fib 0)
; return:     (fib 0)
; return:   (fib 2)
;  stack: fib-done | 1
;   call:   (fib 1)
; return:   (fib 1)
; return: (fib 3)
;  stack:

;;;;;;;;;;;;;;;;;
;;; Exercise 5.6
;;;;;;;;;;;;;;;;;

(define fib-machine
  (make-machine
   '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '((assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
;      (restore continue)			; ***
      (assign n (op -) (reg n) (const 2))
;      (save continue)				; ***
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 11)

(start fib-machine)

(assert '(get-register-contents fib-machine 'val) 89)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.7
;;;;;;;;;;;;;;;;;

; been there done that

;;;;;;;;;;;;;;;;;
;;; Exercise 5.8
;;;;;;;;;;;;;;;;;

(define machine-5.8
  (make-machine
   '(a)
   '()
   '(
   start
     (goto (label here))
   here
     (assign a (const 3))
     (goto (label there))
   here
     (assign a (const 4))
     (goto (label there))
   there)))

(start machine-5.8)

; With the simulator as written, what will the contents
; of register "a" be when control reaches "there"?
(assert '(get-register-contents machine-5.8 'a) 3)

; Modify the extract-labels procedure so that the
; assembler will signal an error if the same label
; name is used to indicate two different locations. 

(define (extract-labels text receive)
  (if (null? text)
      (receive '() '())
      (extract-labels (cdr text)
       (lambda (insts labels)
         (let ((next-inst (car text)))
           (cond ((not (symbol? next-inst))
		  (receive (cons (make-instruction next-inst) insts) labels))
		 ((assoc next-inst labels)
		  (error "Duplicate label -- EXTRACT-LABELS" next-inst))
		 (else
		  (receive insts 
		      (cons (make-label-entry next-inst insts) labels)))))))))

; defining machine-5.8 again should fail with new extract-labels

;;;;;;;;;;;;;;;;;
;;; Exercise 5.9
;;;;;;;;;;;;;;;;;

(define machine-5.9
  (make-machine
   '(a)
   (list (list 'list list))
   '((assign a (op list) (const 1) (label end))
   end)))

(start machine-5.9)

(define (make-simple-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (make-primitive-exp exp machine labels)
  (cond ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))	
        (else (make-simple-primitive-exp exp machine labels))))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-simple-primitive-exp e machine labels)) ; ***
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

; defining and running machine-5.9 should fail with new make-operation-exp

;;;;;;;;;;;;;;;;;
;;; Exercise 5.10
;;;;;;;;;;;;;;;;;

(define (make-execution-procedure inst labels machine
                                  pc flag stack ops)
  (cond ((eq? (car inst) 'set)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'cmp)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'b) ; branch-if-true
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'jmp)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'push)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'pop)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'do)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'o)))

(define (label-exp? exp) (tagged-list? exp 'l))

(define (constant-exp? exp) (tagged-list? exp 'c))

(define (register-exp? exp) (tagged-list? exp 'r))

(define new-fib-machine
  (make-machine
   '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '((set continue (l fib-done))
    fib-loop
      (cmp (o <) (r n) (c 2))
      (b (l immediate-answer))
      (push continue)
      (set continue (l afterfib-n-1))
      (push n)
      (set n (o -) (r n) (c 1))
      (jmp (l fib-loop))
    afterfib-n-1
      (pop n)
      (set n (o -) (r n) (c 2))
      (set continue (l afterfib-n-2))
      (push val)
      (jmp (l fib-loop))
    afterfib-n-2
      (set n (r val))
      (pop val)
      (pop continue)
      (set val (o +) (r val) (r n))
      (jmp (r continue))
    immediate-answer
      (set val (r n))
      (jmp (r continue))
      fib-done)))

(set-register-contents! new-fib-machine 'n 12)

(start new-fib-machine)

(assert '(get-register-contents new-fib-machine 'val) 144)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.11
;;;;;;;;;;;;;;;;;

(load "regsim") ; restore original syntax

;;; A.

(define reduced-fib-machine
  (make-machine
   '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '((assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
      (assign n (op -) (reg n) (const 2))
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
;      (assign n (reg val))		; ***
;      (restore val)			; ***
      (restore n)			; ***
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
      fib-done)))

(set-register-contents! reduced-fib-machine 'n 10)

(start reduced-fib-machine)

(assert '(get-register-contents reduced-fib-machine 'val) 55)

;;; B.

(define machine-5.11-b
  (make-machine
   '(a b)
   (list (list 'debug debug))
   '((save a)     
     (restore b)
   end)))

(start machine-5.11-b)

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name)))
    (lambda ()
      (push stack (cons reg-name (get-contents reg)))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name)))
    (lambda ()
      (let ((element (pop stack)))
	(if (eq? reg-name (car element))
	    (set-contents! reg (cdr element))
	    (error "register name mismatch MAKE-RESTORE" reg-name)))
      (advance-pc pc))))

; defining and running machine-5.11-b again should fail at this point

;;; C.

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let* ((the-ops							; ***
           (list (list 'initialize-stack
                       (lambda ()
			 (for-each init-register-stack register-table)	; ***
			 (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table 
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name) (make-stack))	; ***
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (register-stack name)					; ***
        (let ((val (assoc name register-table)))			; ***
          (if val							; ***
              (get-register-stack val)					; ***
              (error "Unknown register:" name))))			; ***
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'register-stack) register-stack)		; ***
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (get-register-stack reg-table-entry)
  (caddr reg-table-entry))

(define (init-register-stack reg-table-entry)
  ((get-register-stack reg-table-entry) 'initialize))

(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name))
	 (stack ((machine 'register-stack) reg-name)))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
	 (reg (get-register machine reg-name))
	 (stack ((machine 'register-stack) reg-name)))
    (lambda ()
      (set-contents! reg (pop stack))    
      (advance-pc pc))))

(define machine-5.11-c
  (make-machine
   '(a b)
   (list (list 'debug debug))
   '((assign a (const 1))
     (assign b (const 2))
     (save a)
     (save b)
     (assign a (const 3))
     (assign a (const 4))
     (restore a)
     (restore b)
   end)))

(start machine-5.11-c)

(assert '(get-register-contents machine-5.11-c 'a) 1)
(assert '(get-register-contents machine-5.11-c 'b) 2)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.12
;;;;;;;;;;;;;;;;;

(load "regsim") ; restore original syntax

(define (remove-duplicates seq)
  (cond ((null? seq) seq)
	((member (car seq) (cdr seq)) (remove-duplicates (cdr seq)))
	(else (cons (car seq) (remove-duplicates (cdr seq))))))

(define (remove-if pred seq)
  (cond ((null? seq) seq)
	((pred (car seq)) (remove-if pred (cdr seq)))
	(else (cons (car seq) (remove-if pred (cdr seq))))))

(define (symbol<? a b)
  (string<?
   (symbol->string a)
   (symbol->string b)))

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)    
    ((machine 'install-statistics)
     (gather-statistics controller-text))
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
	(statistics '(() () () ()))					; ***
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register:" name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
	      ((eq? message 'install-statistics)			; ***
               (lambda (seq) (set! statistics seq)))			; ***
	      ((eq? message 'get-statistics)				; ***
               statistics)						; ***
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (gather-statistics text)
  (define (all-the-regs)
    (define (inspect item regs)
      (cond ((not (pair? item)) regs)
	    ((eq? 'reg (car item))
	     (cons item regs))
	    (else (inspect (cdr item) (inspect (car item) regs)))))
    (remove-duplicates (inspect text '())))
  (define (is-not-save-or-restore instr)
    (not (and (pair? instr)
	      (or (eq? (car instr) 'save)
		  (eq? (car instr) 'restore)))))
  (define (is-not-goto-with-reg instr)
    (not (and (pair? instr)
	      (eq? (car instr) 'goto)
	      (eq? (car (cadr instr)) 'reg))))
  (define (is-not-assignment instr)
    (not (and (pair? instr)
	      (eq? (car instr) 'assign))))
  (let ((without-labels (remove-if symbol? text)))
    (list 
   ; a list of all instructions, with duplicates removed,
   ; sorted by instruction type (assign, goto, and so on);
     (sort (remove-duplicates without-labels)
	   (lambda (x y) (symbol<? (car x) (car y))))

   ; a list (without duplicates) of the registers used to hold entry points
   ; (these are the registers referenced by goto instructions);
     (remove-duplicates
      (map cadadr (remove-if is-not-goto-with-reg without-labels)))

   ; a list (without duplicates) of the registers that are saved or restored;
     (remove-duplicates
      (map cadr (remove-if is-not-save-or-restore without-labels)))

   ; for each register, a list (without duplicates) of the sources from which
   ; it is assigned (for example, the sources for register val in the factorial
   ; machine of figure 5.11 are (const 1) and ((op *) (reg n) (reg val))). 
     (let* ((regs (map (lambda (x) (cons (cadr x) '())) (all-the-regs)))
	    (all-assigns (remove-if is-not-assignment without-labels))	    
	    (assigns (remove-duplicates all-assigns)))
       (define (distribute seq)
	 (if (null? seq)
	     'ok
	     (let ((reg (assoc (cadr (car seq)) regs)))
	       (if (not reg)
		   (error "could not find matching register for assign")
		   (begin
		     (set-cdr! reg (cons (cddr (car seq)) (cdr reg)))
		     (distribute (cdr seq)))))))
       (distribute assigns)
       regs)
   )))

(define original-fib-machine
  (make-machine
   '(n val continue)
    (list (list '< <) (list '- -) (list '+ +))
    '((assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
      (restore continue)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
    fib-done)))

(define (display-list seq)
  (if (null? seq)
      'done
      (begin
	(debug (car seq) "\n")
	(display-list (cdr seq)))))

(define (display-regs seq)
  (if (null? seq)
      'done
      (begin
	(debug "* reg = " (caar seq) "\n")
	(display-list (cdar seq))
	(debug "\n")
	(display-regs (cdr seq)))))

(define (examine-5.12)
  (debug "\n")
  (let ((stats (original-fib-machine 'get-statistics)))
    (debug "list of all instructions:\n\n")
    (display-list (first stats))
    (debug "\nregisters used to hold entry points: " (second stats) "\n")
    (debug "\nregisters that are saved or restored: " (third stats) "\n")
    (debug "\nregister assignment sources:\n\n")
    (display-regs (fourth stats))))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.13
;;;;;;;;;;;;;;;;;

(load "regsim") ; restore original syntax

(define (make-machine ops controller-text)
  (let ((machine (make-new-machine)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
	      (begin
		(set! register-table
		      (cons (list name (make-register name))
			    register-table))
		(lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define new-sqrt-machine
  (make-machine
   (list (list '/ /) (list '* *) (list '+ +)  (list '- -) (list '> >))
   '((assign guess (const 1.0))
   next-guess
     (assign tmp (op *) (reg guess) (reg guess))
     (assign tmp (op -) (reg tmp) (reg x))
     (test (op >) (reg tmp) (const 0.0))
     (branch (label do-not-invert))
     (assign tmp (op -) (const 0.0) (reg tmp))
   do-not-invert
     (test (op >) (const 0.0001) (reg tmp))
     (branch (label sqrt-done))
     (assign tmp (op /) (reg x) (reg guess))
     (assign tmp (op +) (reg tmp) (reg guess))
     (assign guess (op *) (const 0.5) (reg tmp))
     (goto (label next-guess))
   sqrt-done)))

(set-register-contents! new-sqrt-machine 'x 2.0)

(start new-sqrt-machine)

(assert '(get-register-contents new-sqrt-machine 'guess) 
	(lambda (x) (< 1.4141 x 1.4143)))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.14
;;;;;;;;;;;;;;;;;

(define recursive-factorial-machine
  (make-machine
   (list (list '* *) (list '- -) (list '= =) (list '> >) (list 'debug debug))
   '(restart
     (assign n (reg times))
     (perform (op initialize-stack))
     (assign continue (label fact-done))     ; set up final return address
   fact-loop
     (test (op =) (reg n) (const 1))
     (branch (label base-case))
     ;; Set up for the recursive call by saving n and continue.
     ;; Set up continue so that the computation will continue
     ;; at after-fact when the subroutine returns.
     (save continue)
     (save n)
     (assign n (op -) (reg n) (const 1))
     (assign continue (label after-fact))
     (goto (label fact-loop))
   after-fact
     (restore n)
     (restore continue)
     (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
     (goto (reg continue))                   ; return to caller
   base-case
     (assign val (const 1))                  ; base case: 1! = 1
     (goto (reg continue))                   ; return to caller
   fact-done
     (perform (op debug) (const "\nn = ") (reg times))
     (perform (op print-stack-statistics))
     (assign times (op -) (reg times) (const 1))
     (test (op >) (reg times) (const 0))
     (branch (label restart)))))

(define (check-5.14)
  (set-register-contents! recursive-factorial-machine 'times 7)
  (start recursive-factorial-machine))

;n = 7
;(total-pushes = 12 maximum-depth = 12)
;n = 6
;(total-pushes = 10 maximum-depth = 10)
;n = 5
;(total-pushes = 8 maximum-depth = 8)
;n = 4
;(total-pushes = 6 maximum-depth = 6)
;n = 3
;(total-pushes = 4 maximum-depth = 4)
;n = 2
;(total-pushes = 2 maximum-depth = 2)
;n = 1
;(total-pushes = 0 maximum-depth = 0)

; total-pushes == maximum-depth == 2 * (n - 1)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.15
;;;;;;;;;;;;;;;;;

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
	(instruction-count 0)						; ***
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
	      (begin
		(set! register-table
		      (cons (list name (make-register name))
			    register-table))
		(lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
		(set! instruction-count (+ instruction-count 1))	; ***
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'get-instruction-count) instruction-count)	; ***
              ((eq? message 'reset-instruction-count)			; ***
	       (set! instruction-count 0))				; ***
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define count-machine
  (make-machine
    (list (list '= =) (list '- -))
    '(loop
       (test (op =) (reg n) (const 0))
       (branch (label end))
       (assign n (op -) (reg n) (const 1))
       (goto (label loop))
      end)))

(set-register-contents! count-machine 'n 13)
(start count-machine)
(assert '(count-machine 'get-instruction-count) (+ 2 (* 13 4)))

(count-machine 'reset-instruction-count)

(set-register-contents! count-machine 'n 15)
(start count-machine)
(assert '(count-machine 'get-instruction-count) (+ 2 (* 15 4)))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.16
;;;;;;;;;;;;;;;;;

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
	(trace-enable false)						; ***
	(instruction-count 0)
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
	      (begin
		(set! register-table
		      (cons (list name (make-register name))
			    register-table))
		(lookup-register name)))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
		(if trace-enable					; ***
		    (debug "\n" (instruction-text (car insts))))	; ***
		(set! instruction-count (+ instruction-count 1))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'trace-on) (set! trace-enable true))	; ***
              ((eq? message 'trace-off) (set! trace-enable false))	; ***
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'get-instruction-count) instruction-count)
              ((eq? message 'reset-instruction-count)
	       (set! instruction-count 0))
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.17
;;;;;;;;;;;;;;;;;

(define (assemble controller-text machine)
  (extract-labels controller-text
    (lambda (insts labels)
      ((machine 'install-labels) labels)				; ***
      (update-insts! insts labels machine)
      insts)))

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
	(trace-enable false)
	(labels nil)							; ***
	(instruction-count 0)
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
	      (begin
		(set! register-table
		      (cons (list name (make-register name))
			    register-table))
		(lookup-register name)))))
      (define (print-label labels insts)				; ***
	(cond ((null? labels) 'meh)					; ***
	      ((eq? insts (cdar labels))				; ***
	       (debug "\n" (caar labels) ":"))				; ***
	      (else (print-label (cdr labels) insts))))			; ***
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (begin
		(if trace-enable
		    (begin
		      (print-label labels insts)			; ***
		      (debug "\n  " (instruction-text (car insts)))))
		(set! instruction-count (+ instruction-count 1))
                ((instruction-execution-proc (car insts)))
                (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-labels)				; ***
               (lambda (seq) (set! labels seq)))			; ***
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'trace-on) (set! trace-enable true))
              ((eq? message 'trace-off) (set! trace-enable false))
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'get-instruction-count) instruction-count)
              ((eq? message 'reset-instruction-count)
	       (set! instruction-count 0))
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.18
;;;;;;;;;;;;;;;;;

(define (make-register name)
  (let ((contents '*unassigned*)
	(trace-enable false))
    (define (print-reg value)
      (debug "\nregister = " name 
	     ", value = " contents
	     ", is being set to " value))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) 
	       (cond ((eq? value 'trace-register)
		      (set! trace-enable true))
		     ((eq? value 'untrace-register) 
		      (set! trace-enable false))
		     (else 
		      (if trace-enable (print-reg value))
		      (set! contents value)))))
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define simple-machine
  (make-machine
    (list (list '= =) (list 'div quotient))
    '((assign n (const trace-register))
      (assign n (const 256))
    loop
      (test (op =) (reg n) (const 0))
      (branch (label end))
      (assign n (op div) (reg n) (const 2))
      (goto (label loop))
    end)))

; (start simple-machine)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.19
;;;;;;;;;;;;;;;;;

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
	(trace-enable false)
	(labels nil)
	(breakpoints nil)
	(instruction-count 0)
	(skip-next-breakpoint false)
        (the-instruction-sequence '()))
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;;**next for monitored stack (as in section 5.2.4)
                 ;;  -- comment out if not wanted
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
	      (begin
		(set! register-table
		      (cons (list name (make-register name))
			    register-table))
		(lookup-register name)))))
      (define (print-label labels insts)
	(cond ((null? labels) 'meh)
	      ((eq? insts (cdar labels))
	       (debug "\n" (caar labels) ":"))
	      (else (print-label (cdr labels) insts))))
      (define (should-break? insts)
	(let ((break (assoc insts breakpoints)))
	  (if (or skip-next-breakpoint (not break))
	      (begin
		(set! skip-next-breakpoint false)
		false)
	      (begin
		(debug "\nbreakpoint @ " (cadr break) " + " (caddr break) "\n")
		'break))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (cond ((null? insts) 'done)
		((should-break? insts))
		(else
		 (if trace-enable
		     (begin
		       (print-label labels insts)
		       (debug "\n  " (instruction-text (car insts)))))
		 (set! instruction-count (+ instruction-count 1))
		 ((instruction-execution-proc (car insts)))
		 (execute)))))
      (define (get-breakpoint instr n)
	(if (= n 0)
	    instr
	    (get-breakpoint (cdr instr) (- n 1))))
      (define (set-breakpoint label n)
	(let ((entry (assoc label labels)))
	  (if (not entry)
	      (error "setting breakpoint to unexisting label" label)
	      (set! breakpoints 
		    (cons (list (get-breakpoint (cdr entry) (max 0 (- n 1)))
				label n)
			  breakpoints)))))
      (define (remove-breakpoint label n)
	(define (pred x)
	  (and (eq? label (cadr x))
	       (= n (caddr x))))
	(set! breakpoints (remove-if pred breakpoints))
	'ok)
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
	      ((eq? message 'continue)
               (set! skip-next-breakpoint true)
               (execute))
	      ((eq? message 'cancel-all-breakpoints)
	       (set! breakpoints nil)
	       'ok)
              ((eq? message 'install-labels)
               (lambda (seq) (set! labels seq)))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'trace-on) (set! trace-enable true))
              ((eq? message 'trace-off) (set! trace-enable false))
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'set-breakpoint) set-breakpoint)
              ((eq? message 'remove-breakpoint) remove-breakpoint)
              ((eq? message 'get-instruction-count) instruction-count)
              ((eq? message 'reset-instruction-count)
	       (set! instruction-count 0))
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (cancel-breakpoint machine label n)
  ((machine 'remove-breakpoint) label n))

(define (proceed-machine machine)
  (machine 'continue))

(define (cancel-all-breakpoints machine)
  (machine 'cancel-all-breakpoints))

(define gcd-machine
  (make-machine
   (list (list 'rem remainder) (list '= =))
   '(test-b
       (test (op =) (reg b) (const 0))
       (branch (label gcd-done))
       (assign t (op rem) (reg a) (reg b))
       (assign a (reg b))
       (assign b (reg t))
       (goto (label test-b))
     gcd-done)))

(gcd-machine 'trace-on)
(set-register-contents! gcd-machine 'a 36)
(set-register-contents! gcd-machine 'b 60)
(set-breakpoint gcd-machine 'test-b 4)
;(start gcd-machine)
;(cancel-all-breakpoints gcd-machine)
;(proceed-machine gcd-machine)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.20
;;;;;;;;;;;;;;;;;

; see ex-5.20.png

;;;;;;;;;;;;;;;;;
;;; Exercise 5.21
;;;;;;;;;;;;;;;;;

; A.

(define count-leaves-machine
  (make-machine
   (list (list 'null? null?)
	 (list 'pair? pair?)
	 (list 'car car)
	 (list 'cdr cdr)
	 (list '+ +))
   '((assign continue (label count-leaves-done))
   count-leaves
     (test (op null?) (reg tree))
     (branch (label empty-count-leaves))
     (test (op pair?) (reg tree))
     (branch (label pair-leaves))
     (goto (label atom-leaf))
   pair-leaves
     (save continue)
     (save tree)
     (assign continue (label after-count-car))
     (assign tree (op car) (reg tree))
     (goto (label count-leaves))
   after-count-car
     (restore tree)
     (save count)
     (assign continue (label after-count-cdr))
     (assign tree (op cdr) (reg tree))
     (goto (label count-leaves))
   after-count-cdr
     (restore car-count)
     (assign count (op +) (reg count) (reg car-count))
     (restore continue)
     (goto (reg continue))
   atom-leaf
     (assign count (const 1))
     (goto (reg continue))
   empty-count-leaves
     (assign count (const 0))
     (goto (reg continue))
   count-leaves-done)))

(set-register-contents! count-leaves-machine 'tree '(1 2 (a b (c d) e (f) 5)))
(start count-leaves-machine)

(assert '(get-register-contents count-leaves-machine 'count) 9)

; B.

(define count-leaves-machine
  (make-machine
   (list (list 'null? null?)
	 (list 'pair? pair?)
	 (list 'car car)
	 (list 'cdr cdr)
	 (list '+ +))
   '((assign count (const 0))
     (assign continue (label count-leaves-done))
   count-leaves
     (test (op null?) (reg tree))
     (branch (label count-leaves-return))
     (test (op pair?) (reg tree))
     (branch (label pair-leaves))
     (goto (label atom-leaf))
   pair-leaves
     (save continue)
     (save tree)
     (assign continue (label after-count-car))
     (assign tree (op car) (reg tree))
     (goto (label count-leaves))
   after-count-car
     (restore tree)
     (assign continue (label after-count-cdr))
     (assign tree (op cdr) (reg tree))
     (goto (label count-leaves))
   after-count-cdr
     (restore continue)
     (goto (reg continue))
   atom-leaf
     (assign count (op +) (reg count) (const 1))
   count-leaves-return
     (goto (reg continue))
   count-leaves-done)))

(set-register-contents! count-leaves-machine 'tree '((1 (2 (3) 4 (5) 6) 7) 8))
(start count-leaves-machine)

(assert '(get-register-contents count-leaves-machine 'count) 8)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.22
;;;;;;;;;;;;;;;;;

(define append-machine
  (make-machine
   (list (list 'null? null?)
	 (list 'cons cons)
	 (list 'car car)
	 (list 'cdr cdr))
   '((assign continue (label append-done))
   append
     (test (op null?) (reg a))
     (branch (label finish))
     (save continue)
     (assign continue (label sub-append-done))
     (save a)
     (assign a (op cdr) (reg a))
     (goto (label append))
   sub-append-done
     (restore a)
     (assign a (op car) (reg a))
     (assign ret (op cons) (reg a) (reg ret))
     (restore continue)
     (goto (reg continue))
   finish
     (assign ret (reg b))
     (goto (reg continue))
   append-done)))

(set-register-contents! append-machine 'a '(1 (2) 3))
(set-register-contents! append-machine 'b '(a (b c)))
(start append-machine)

(assert '(get-register-contents append-machine 'ret) '(1 (2) 3 a (b c)))

(define append!-machine
  (make-machine
   (list (list 'set-cdr! set-cdr!)
	 (list 'null? null?)	 
	 (list 'car car)
	 (list 'cdr cdr))
   '((test (op null?) (reg a))
     (branch (label finish))
     (assign i (reg a))
   append
     (assign j (op cdr) (reg i))
     (test (op null?) (reg j))
     (branch (label set-cdr))
     (assign i (reg j))
     (goto (label append))
   set-cdr
     (assign i (op set-cdr!) (reg i) (reg b))
     (goto (label append-done))
   finish
     (assign a (reg b))
   append-done)))

(set-register-contents! append!-machine 'a '(1 2 3 4))
(set-register-contents! append!-machine 'b '(a b c d))
(start append!-machine)

(assert '(get-register-contents append!-machine 'a) '(1 2 3 4 a b c d))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.23
;;;;;;;;;;;;;;;;;

(define make-new-machine-modern make-new-machine)

(define (make-machine-modern ops controller-text)
  (let ((machine (make-new-machine-modern)))
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(load "evaluator")

(load "regsim") ; restore original syntax

(define (let? exp) (tagged-list? exp 'let))

(define (cond? exp) (tagged-list? exp 'cond))

(define (let->lambda let-form)
  (cons
   (cons 'lambda 
	 (cons (map car (cadr let-form))	; arguments
	       (cddr let-form)))		; body
   (map cadr (cadr let-form))))			; parameters

(load "eceval")

;;;;;;;;;;;;;;;;;
;;; Exercise 5.24
;;;;;;;;;;;;;;;;;

; implemented in eceval.scm

;;;;;;;;;;;;;;;;;
;;; Exercise 5.25
;;;;;;;;;;;;;;;;;

(load "lazy-eceval")

;;;;;;;;;;;;;;;;;
;;; Exercise 5.26
;;;;;;;;;;;;;;;;;

; A. You will find that the maximum depth required to
;    evaluate n! is independent of n. What is that depth?

; maximum-depth = 10

; B. Determine from your data a formula in terms of n for the total
;    number of push operations used in evaluating n! for any n > 1.

; total-pushes = 35 * n + 29

;;;;;;;;;;;;;;;;;
;;; Exercise 5.27
;;;;;;;;;;;;;;;;;

;                     | maximum-depth | total-pushes |
;---------------------+---------------+--------------+
; recursive factorial |   5 * n + 3   | 32 * n - 16  |
;                     |               |              |
; iterative factorial |      10       | 35 * n + 29  |

;;;;;;;;;;;;;;;;;
;;; Exercise 5.28
;;;;;;;;;;;;;;;;;

; without tail recursion:

;                     | maximum-depth | total-pushes |
;---------------------+---------------+--------------+
; recursive factorial |   8 * n + 3   | 34 * n - 16  |
;                     |               |              |
; iterative factorial |  3 * n + 14   | 37 * n + 33  |

;;;;;;;;;;;;;;;;;
;;; Exercise 5.29
;;;;;;;;;;;;;;;;;

; A.

; maximum-depth = 5 * n + 3

; B.

; total-pushes fib(n) = total-pushes fib(n - 1) + total-pushes fib(n - 2) + 40
; k = 40

; fib(5) * a + b = 240 (pushes for fib(4))
; fib(4) * a + b = 128 (pushes for fib(3))

; 5 * a + b = 240
; 3 * a + b = 128

; total-pushes fib(n) = 56 * fib(n + 1) - 40
; a = 56, b = -40

;;;;;;;;;;;;;;;;;
;;; Exercise 5.30
;;;;;;;;;;;;;;;;;

; changes for this exercise are in "eceval.scm" and are marked with "ex 5.30"

; (also lazy evaluator was updated with same changes)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.31
;;;;;;;;;;;;;;;;;

;(1) saves and restores the env register around the evaluation of the operator
;(2) saves and restores env around the evaluation of each operand (except last)
;(3) saves and restores argl around the evaluation of each operand
;(4) saves and restores proc around the evaluation of the operand sequence

; For each of the following combinations, say which of these save and restore
; operations are superfluous and thus could be eliminated by the
; compiler's preserving mechanism:

; (f 'x 'y) ; (1), (2), (3) and (4) are superfluous

; ((f) 'x 'y) ; (2), (3) and (4) are superfluous

; (f (g 'x) y) ; (1) are superfluous (we need to save env to lookup y) 

; (f (g 'x) 'y) ; (1), (2) are superfluous

;;;;;;;;;;;;;;;;;
;;; Exercise 5.32
;;;;;;;;;;;;;;;;;

; A. changes are in "eceval.scm" and are marked with "ex 5.32" 

; (+ 1 2) without optimization is (total-pushes = 8 maximum-depth = 5)

; (+ 1 2) with optimization is (total-pushes = 6 maximum-depth = 5)

; B. I don't think so, because
;    "extending the evaluator to recognize more and more special cases"
;    would save some "saves" and "restores", but it will mess up evaluator
;    with lots of code for "checking" special cases that would be run
;    every time expression is evaluated.
;
;    For compilation these checks are done at compile time.

;;;;;;;;;;;;;;;;;
;;; Exercise 5.33
;;;;;;;;;;;;;;;;;

(load "compiler")

(define (format-code seq)
  (if (null? seq)
      'done
      (begin
	(let ((x (car seq)))
	  (if (symbol? x)
	      (debug x "\n")
	      (debug "  " x "\n")))
	(format-code (cdr seq)))))

; a.txt = (* (factorial (- n 1)) n)
; b.txt = (* n (factorial-alt (- n 1)))

;--- a.scm       2010-12-05 18:19:03.000000000 +0200
;+++ b.scm       2010-12-05 18:22:28.000000000 +0200
;@@ -30,9 +30,7 @@
;   (assign proc (op lookup-variable-value) (const *) (reg env))
;   (save continue)
;   (save proc)
;-  (assign val (op lookup-variable-value) (const n) (reg env))
;-  (assign argl (op list) (reg val))
;-  (save argl)
;+  (save env)
;   (assign proc (op lookup-variable-value) (const factorial) (reg env))
;   (save proc)
;   (assign proc (op lookup-variable-value) (const -) (reg env))
;@@ -60,7 +58,9 @@
; primitive-branch11
;   (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
; after-call9
;-  (restore argl)
;+  (assign argl (op list) (reg val))
;+  (restore env)
;+  (assign val (op lookup-variable-value) (const n) (reg env))
;   (assign argl (op cons) (reg val) (reg argl))
;   (restore proc)
;   (restore continue)

; Does either program execute more efficiently than the other? 

; no, + 2 assigns, 1 save, 1 restore
;     - 2 assigns, 1 save, 1 restore

; Explain any differences you find.

; in case of (* (factorial (- n 1)) n) we save "argl" around factorial,
; because we first look up "n" then calculate factorial

; in case of (* n (factorial-alt (- n 1))) we save "env" around factorial,
; because we first calculate factorial then look up "n"

;;;;;;;;;;;;;;;;;
;;; Exercise 5.34
;;;;;;;;;;;;;;;;;

(define iterative-factorial-code
  (compile
   '(define (factorial n)
      (define (iter product counter)
	(if (> counter n)
	    product
	    (iter (* counter product)
		  (+ counter 1))))
      (iter 1 1))
   the-empty-environment
   'val
   'next))

(define (dump-5.33)
  (format-code (statements iterative-factorial-code)))

;; ********************************************
;; commented code

#|
;; define factorial, skip over code
  (assign val (op make-compiled-procedure) (label entry2) (reg env))
  (goto (label after-lambda1))

;; factorial body
entry2
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

;; define iter, skip over code
  (assign val (op make-compiled-procedure) (label entry7) (reg env))
  (goto (label after-lambda6))

;; iter body
entry7
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (product counter)) (reg argl) (reg env))

;; calculate (> counter n)
  (save continue)
  (save env)
  (assign proc (op lookup-variable-value) (const >) (reg env))
  (assign val (op lookup-variable-value) (const n) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
(assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
  (restore env)
  (restore continue)

;; test (> counter n) result
  (test (op false?) (reg val))
  (branch (label false-branch9))

;; consequent, return product
true-branch10
  (assign val (op lookup-variable-value) (const product) (reg env))
  (goto (reg continue))

;; alternative, (iter (* counter product) (+ counter 1))))
false-branch9
  (assign proc (op lookup-variable-value) (const iter) (reg env))

;; calculate (+ counter 1)
  (save continue)
  (save proc)
  (save env)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch16))
compiled-branch15
  (assign continue (label after-call14))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch16
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call14
  (assign argl (op list) (reg val))
  (restore env)

;; calculate (* counter product)
  (save argl)
  (assign proc (op lookup-variable-value) (const *) (reg env))
  (assign val (op lookup-variable-value) (const product) (reg env))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const counter) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch13))
compiled-branch12
  (assign continue (label after-call11))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch13
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call11
  (restore argl)
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)

;; apply iter
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call17
after-if8

;; assign the procedure to the variable iter
after-lambda6
  (perform (op define-variable!) (const iter) (reg val) (reg env))
  (assign val (const ok))

;; call (iter 1 1)
  (assign proc (op lookup-variable-value) (const iter) (reg env))
  (assign val (const 1))
  (assign argl (op list) (reg val))
  (assign val (const 1))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch5))
compiled-branch4
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch5
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call3

;; assign the procedure to the variable factorial
after-lambda1
  (perform (op define-variable!) (const factorial) (reg val) (reg env))
  (assign val (const ok))
|#

;; ********************************************
;; egrep "(;;|continue)" on above code

#|
;; define factorial, skip over code
;; factorial body
;; define iter, skip over code
;; iter body
;; calculate (> counter n)
  (save continue)						    ; save 1
  (assign continue (label after-call20))
  (restore continue)						    ; restore 1
;; test (> counter n) result
;; consequent, return product
  (goto (reg continue))
;; alternative, (iter (* counter product) (+ counter 1))))
;; calculate (+ counter 1)
  (save continue)						    ; save 2
  (assign continue (label after-call14))
;; calculate (* counter product)
  (assign continue (label after-call11))
  (restore continue)						    ; restore 2
;; apply iter

						    ; iter is called here
						    ; without continue on stack

  (goto (reg continue))
;; assign the procedure to the variable iter
;; call (iter 1 1)
  (goto (reg continue))
;; assign the procedure to the variable factorial
|#

;; ********************************************
;; egrep "(;;|continue)" on recursive book code

#|
;; construct the procedure and skip over code for the procedure body
entry2     ; calls to factorial will enter here
;; begin actual procedure body
  (save continue)						    ; save 1
;; compute (= n 1)
after-call15   ; val now contains result of (= n 1)
  (restore continue)						    ; restore 1
true-branch5  ; return 1
  (goto (reg continue))
;; compute and return (* (factorial (- n 1)) n)
  (save continue)						    ; save 2
  (save proc)   ; save * procedure
  (save argl)   ; save partial argument list for *
;; compute (factorial (- n 1)), which is the other argument for *
  (save proc)  ; save factorial procedure
;; compute (- n 1), which is the argument for factorial
after-call6   ; val now contains result of (- n 1)
  (restore proc) ; restore factorial
;; apply factorial

						     ; factorial is called here
						     ; with continue on stack
						     ; saved by "save 2"

after-call9      ; val now contains result of (factorial (- n 1))
  (restore argl) ; restore partial argument list for *
  (restore proc) ; restore *
  (restore continue)						    ; restore 2
;; apply * and return its value
;; note that a compound procedure here is called tail-recursively
  (goto (reg continue))
;; assign the procedure to the variable factorial
|#

;;;;;;;;;;;;;;;;;
;;; Exercise 5.35
;;;;;;;;;;;;;;;;;

#|
  (assign val (op make-compiled-procedure) (label entry16) (reg env))
  (goto (label after-lambda15))
entry16
  (assign env (op compiled-procedure-env) (reg proc))
  (assign env (op extend-environment) (const (x)) (reg argl) (reg env))

;; (+ x (g (+ x 2)))
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (save continue)
  (save proc)
  (save env)

;; (g (+ x 2))
  (assign proc (op lookup-variable-value) (const g) (reg env))
  (save proc)

;; (+ x 2)
  (assign proc (op lookup-variable-value) (const +) (reg env))
  (assign val (const 2))
  (assign argl (op list) (reg val))
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch19))
compiled-branch18
  (assign continue (label after-call17))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch19
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call17
;; done (+ x 2)

  (assign argl (op list) (reg val))
  (restore proc)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch22))
compiled-branch21
  (assign continue (label after-call20))
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch22
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call20
;; done (g (+ x 2))

  (assign argl (op list) (reg val))
  (restore env)
  (assign val (op lookup-variable-value) (const x) (reg env))
  (assign argl (op cons) (reg val) (reg argl))
  (restore proc)
  (restore continue)
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-branch25))
compiled-branch24
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))
primitive-branch25
  (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
  (goto (reg continue))
after-call23
;; done (+ x (g (+ x 2)))

after-lambda15
  (perform (op define-variable!) (const f) (reg val) (reg env))
  (assign val (const ok))
|#

;; (define (f ?)
;;   (+ x (g (+ x 2))))

;; perhaps arguments are x and g

;;;;;;;;;;;;;;;;;
;;; Exercise 5.36
;;;;;;;;;;;;;;;;;

(load "eceval-ok")

(define (test-compiler code)
  (define mach
    (make-machine-modern
     eceval-operations
     (append
      '((assign env (op get-global-environment)))
      (statements (compile code the-empty-environment 'val 'next)))))
  (start mach)
  (get-register-contents mach 'val))

; uncomment "ex 5.36" marks in "compiler.scm" to get "left-to-right" style
(define (test-5.36)
  (test-compiler
   '(begin
      (> (begin (debug "left")  1)
	 (begin (debug " to ")  1)
	 (begin (debug "right") 1))
      (debug "\n")
      'ok)))

;; What order of evaluation does our compiler
;; produce for operands of a combination? 
;; Is it left-to-right, right-to-left, or some other order?

; "right to left"

;; Where in the compiler is this order determined?

; in procedures "construct-arglist" and "code-to-get-rest-args"

; operands are reversed in "construct-arglist" to be compiled
; from end of list so that results could be conveniently consed
; together in "code-to-get-rest-args"

;; How does changing the order of operand evaluation affect
;; the efficiency of the code that constructs the argument list?

; adjoining result of operand to the end of argument list is O(n)
; (it is O(n) for each individual arg, to make whole arglist it is O(n^2)

; while consing result onto argument list O(1)
; (reverse is O(n) for all arglist, but happens at compile time)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.37
;;;;;;;;;;;;;;;;;

; to enable dumb "preserving" procedure comment in
; code block marked with (ex 5.37) in "compiler.scm"

(define (dump-5.37)
  (format-code
   (statements
    (compile 
     '(define (fib n)
	(if (< n 2)
	    n
	    (+ (fib (- n 1)) (fib (- n 2))))) 
     the-empty-environment
     'val
     'next))))

;; Compare the code to that generated with the preserving mechanism intact. 

;arturs@sekvoja:~/Sources/misc/sicp$ diff good.scm bad.scm -u | grep '^- ' | wc
;      0       0       0
;arturs@sekvoja:~/Sources/misc/sicp$ diff good.scm bad.scm -u | grep '^+ ' | wc
;     96     288    1820

; for simple fibonacci numbers routine extra 96 save/restore instructions

;arturs@sekvoja:~/Sources/misc/sicp$ wc good.scm 
; 107  439 3756 good.scm
;arturs@sekvoja:~/Sources/misc/sicp$ wc bad.scm 
; 203  631 5480 bad.scm

; it almost doubles amount of instructions

;;;;;;;;;;;;;;;;;
;;; Exercise 5.38
;;;;;;;;;;;;;;;;;

;;; A.

(define (spread-arguments operands operator-code env)
  (let ((operand-count (length operands)))
    (cond ((= 0 operand-count)
	   operator-code)
	  ((= 1 operand-count)
	   (append-instruction-sequences
	    (compile (first operands) env 'arg1 'next)
	    operator-code))
	  ((= 2 operand-count)
	   (preserving '(env)
	    (compile (first operands) env 'arg1 'next)
	    (preserving '(arg1)
	     (compile (second operands) env 'arg2 'next)
             operator-code)))
	  (else
	   (error "SPREAD-ARGUMENTS -- too many operands for primitive")))))

;;; B.

(define (primitive-operation? exp)
  (or (tagged-list? exp '+)
      (tagged-list? exp '*)
      (tagged-list? exp '-)
      (tagged-list? exp '=)))

(define (compile-operator exp target)
  (define (make-instructions args)
    (list (append `(assign ,target (op ,(operator exp)))
		  (map (lambda (x) (list 'reg x)) args))))
  (define (make-primitive-sequence args)
    (make-instruction-sequence args (list target) (make-instructions args)))
  (let ((count (length (operands exp))))
    (cond ((= count 0) (make-primitive-sequence '()))
	  ((= count 1) (make-primitive-sequence '(arg1)))
	  ((= count 2) (make-primitive-sequence '(arg1 arg2)))
	  (else (error "COMPILE-OPERATOR -- too many operands")))))

(define (compile-primitive-operation exp env target linkage)
  (end-with-linkage linkage
   (spread-arguments (operands exp) (compile-operator exp target) env)))

(define (compile exp env target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((primitive-operation? exp)
	 (compile-primitive-operation exp env target linkage))
        ((variable? exp)
         (compile-variable exp env target linkage))
        ((assignment? exp)
         (compile-assignment exp env target linkage))
        ((definition? exp)
         (compile-definition exp env target linkage))
        ((if? exp) (compile-if exp env target linkage))
        ((lambda? exp) (compile-lambda exp env target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) env target linkage))
        ((cond? exp) (compile (cond->if exp) env target linkage))
        ((application? exp)
         (compile-application exp env target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

;;; C.
  
(define (test-5.38 num)
  (test-compiler 
   `(begin
      (define (factorial n)
	(if (= n 1)
	    1
	    (* (factorial (- n 1)) n)))
      (factorial ,num))))

(assert '(test-5.38 6) 720)

;;; D.

(define previous-compile-primitive-operation compile-primitive-operation)

(define (primitive-operation-many-operands? exp)
  (and (> (length (operands exp)) 2)
       (or (tagged-list? exp '+)
	   (tagged-list? exp '*))))

(define (compile-primitive-operation exp env target linkage)
  (if (not (primitive-operation-many-operands? exp))
      (previous-compile-primitive-operation exp env target linkage)
      (compile-primitive-operation-many-operands exp env target linkage)))

(define (compile-primitive-operation-many-operands exp env target linkage)
  (let ((op (operator exp))
	(1st (first (operands exp)))
	(2nd (second (operands exp)))
	(rest (cddr (operands exp))))
  (compile `(,op (,op ,1st ,2nd) ,@rest) env target linkage)))

; I chose former approach because it does not
; use stack if operands are self-evaluating

; (+ 1 2 3 4) == (+ (+ (+ 1 2) 3) 4)
; produces:

;  (assign arg1 (const 1))
;  (assign arg2 (const 2))
;  (assign arg1 (op +) (reg arg1) (reg arg2))
;  (assign arg2 (const 3))
;  (assign arg1 (op +) (reg arg1) (reg arg2))
;  (assign arg2 (const 4))
;  (assign val (op +) (reg arg1) (reg arg2))

; (+ 1 2 3) == (+ 1 (+ 2 (+ 3)))
; produces:

;  (assign arg1 (const 1))
;  (save arg1)
;  (assign arg1 (const 2))
;  (assign arg2 (const 3))
;  (assign arg2 (op +) (reg arg1) (reg arg2))
;  (restore arg1)
;  (assign val (op +) (reg arg1) (reg arg2))

;;;;;;;;;;;;;;;;;
;;; Exercise 5.39
;;;;;;;;;;;;;;;;;

(define (lexical-address-lookup env addr)
  (let* ((frame (list-ref env (first addr)))
	 (value (list-ref (frame-values frame) (second addr))))
    (if (eq? value '*unassigned*)
	(error "LEXICAL-ADDRESS-LOOKUP access of unassigned variable")
	value)))

(define (n-th seq n)
  (cond ((= n 0) seq)
	((null? seq) false)	
	(else (n-th (cdr seq) (- n 1)))))      

(define (lexical-address-set! env addr value)
  (let* ((frame (list-ref env (first addr)))
	 (value-place (n-th (frame-values frame) (second addr))))
    (if value-place
	(set-car! value-place value)
	(error "LEXICAL-ADDRESS-SET! attempt to set undefined variable"))))

(define test-env
  (extend-environment 
   '(x y z a b c) '(11 12 13 14 15 16)
   (extend-environment 
    '(x y z e) '(10 20 30 *unassigned*)
    (extend-environment
     '(a b c) '(1 2 3)
     the-empty-environment))))
  
(assert '(lexical-address-lookup test-env '(0 0)) 11)

(assert '(lexical-address-lookup test-env '(1 1)) 20)

(assert '(lexical-address-lookup test-env '(2 2)) 3)

(lexical-address-set! test-env '(1 3) 40)

(assert '(lexical-address-lookup test-env '(1 3)) 40)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.40
;;;;;;;;;;;;;;;;;

; changes are in "compiler.scm" and in code above

;;;;;;;;;;;;;;;;;
;;; Exercise 5.41
;;;;;;;;;;;;;;;;;

(define (position item seq)
  (define (count seq num)
    (cond ((null? seq) false)
	  ((eq? item (car seq)) num)
	  (else (count (cdr seq) (+ num 1)))))
  (count seq 0))

(define (find-variable var env)
  (define (find-in env num)
    (if (null? env)
	'not-found
	(let ((pos (position var (car env))))
	  (if pos
	      (list num pos)
	      (find-in (cdr env) (+ num 1))))))
  (find-in env 0))

(assert '(find-variable 'c '((y z) (a b c d e) (x y))) '(1 2))

(assert '(find-variable 'x '((y z) (a b c d e) (x y))) '(2 0))

(assert '(find-variable 'w '((y z) (a b c d e) (x y))) 'not-found)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.42
;;;;;;;;;;;;;;;;;

(define eceval-operations
  (append eceval-operations
	  (list
	   (list 'lexical-address-lookup lexical-address-lookup)
	   (list 'lexical-address-set! lexical-address-set!))))

(define (compile-variable exp env target linkage)
  (let ((addr (find-variable exp env)))
    (end-with-linkage linkage
      (if (eq? addr 'not-found)
	  (make-instruction-sequence '() (list target 'env)
	   `((assign env (op get-global-environment))
	     (assign ,target
		     (op lookup-variable-value)
		     (const ,exp)
		     (reg env))))
	  (make-instruction-sequence '(env) (list target)
	   `((assign ,target
		     (op lexical-address-lookup)
		     (reg env)
		     (const ,addr))))))))

(define (compile-assignment exp env target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) env 'val 'next)))
    (let ((addr (find-variable var env)))
      (end-with-linkage linkage
       (preserving '(env)
        get-value-code
	 (append
	  (if (eq? addr 'not-found)				   
	      (make-instruction-sequence '(val) (list target 'env)
	       `((assign env (op get-global-environment))
		 (perform (op set-variable-value!)
			  (const ,var)
			  (reg val)
			  (reg env))
		 (assign ,target (const ok))))
	      (make-instruction-sequence '(env val) (list target)
	       `((perform (op lexical-address-set!)
			  (reg env)
			  (const ,addr)			
			  (reg val))
		 (assign ,target (const ok)))))))))))

(define (test-5.42)
  (test-compiler 
   `(((lambda (x y)
	(lambda (a b c d e)
	  ((lambda (y z) (* x y z))
	   (* a b x)
	   (+ c d x))))
      3 4)
     1 2 3 4 5)))

(assert '(test-5.42) 180)

(define (test-factorial num)
  (test-compiler 
   `(begin
      (define (factorial n)
	(if (= n 1)
	    1
	    (* (factorial (- n 1)) n)))
      (factorial ,num))))

(assert '(test-factorial 7) 5040)

(define (test-fib num)
  (test-compiler
   `(begin
      (define (fib n)
	(if (< n 2)
	    n
	    (+ (fib (- n 1)) (fib (- n 2)))))
      (fib ,num))))
      
(assert '(test-fib 12) 144)

(define (test-more)
  (test-compiler 
   `((lambda (x)
       (set! x (+ x 5))
       ((lambda (y)
	  ((lambda (x) (set! x 2)) 1) ; should have no effect
	  (set! x (+ x y)))
	2)
       (set! x (+ x 6))
       x)
     0)))

(assert '(test-more) 13)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.43
;;;;;;;;;;;;;;;;;

; procedures "transform-definition", "scan-out-defines" and "attach-at-end"
; are copied one-to-one from chapter4 exercise 4.16

(define (attach-at-end list x)
  (set-cdr! (last-pair list) x)
  list)

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

; procedures "let->combination" is copied one-to-one from chapter4 exercise 4.6

(define (let->combination exp)
  (cons (cons 'lambda 
	      (cons (map car (cadr exp)) 
		    (cddr exp)))
	(map cadr (cadr exp))))

(define (compile-lambda-body exp env proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (scan-out-defines (lambda-body exp))
		       (cons formals env) 'val 'return))))

(define (compile exp env target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((primitive-operation? exp)
	 (compile-primitive-operation exp env target linkage))
        ((variable? exp)
         (compile-variable exp env target linkage))
        ((assignment? exp)
         (compile-assignment exp env target linkage))
        ((definition? exp)
         (compile-definition exp env target linkage))
        ((if? exp) (compile-if exp env target linkage))
        ((lambda? exp) (compile-lambda exp env target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) env target linkage))
        ((cond? exp) (compile (cond->if exp) env target linkage))
        ((let? exp) (compile (let->combination exp) env target linkage))
        ((application? exp)
         (compile-application exp env target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(define (test-iterative-factorial num)
  (test-compiler 
   `(begin
      (define (factorial n)
	(define (iter product counter)
	  (if (> counter n)
	      product
	      (iter (* counter product)
		    (+ counter 1))))
	(iter 1 1))
      (factorial ,num))))

(assert '(test-iterative-factorial 7) 5040)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.44
;;;;;;;;;;;;;;;;;

(define (test-primitives)
  (test-compiler 
   `(begin
      (define (f +)
	(+ 123456 654321))
      (f *))))

(assert '(test-primitives) 777777)

(define (is-good-operation? exp env)
  (and (pair? exp)
       (symbol? (car exp))
       (eq? 'not-found (find-variable (car exp) env))))

(define (primitive-operation? exp env)
  (and (is-good-operation? exp env)
       (or (tagged-list? exp '+)
	   (tagged-list? exp '*)
	   (tagged-list? exp '-)
	   (tagged-list? exp '=))))
  
(define (compile exp env target linkage)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((primitive-operation? exp env)
	 (compile-primitive-operation exp env target linkage))
        ((variable? exp)
         (compile-variable exp env target linkage))
        ((assignment? exp)
         (compile-assignment exp env target linkage))
        ((definition? exp)
         (compile-definition exp env target linkage))
        ((if? exp) (compile-if exp env target linkage))
        ((lambda? exp) (compile-lambda exp env target linkage))
        ((begin? exp)
         (compile-sequence (begin-actions exp) env target linkage))
        ((cond? exp) (compile (cond->if exp) env target linkage))
        ((let? exp) (compile (let->combination exp) env target linkage))
        ((application? exp)
         (compile-application exp env target linkage))
        (else
         (error "Unknown expression type -- COMPILE" exp))))

(assert '(test-primitives) 80779853376)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.45
;;;;;;;;;;;;;;;;;

(make-eceval)

(define (dump-5.45)
  (format-code
   (statements
    (compile
     '(define (factorial n)
	(if (= n 1)
	    1
	    (* (factorial (- n 1)) n)))
     the-empty-environment 'val 'next))))

(define (test-5.45)
  (compile-and-go
   '(define (factorial n)
      (if (= n 1)
	  1
	  (* (factorial (- n 1)) n)))))

(define recursive-factorial-machine
  (make-machine
   '(n continue val)
   (list (list '* *) (list '- -) (list '= =) (list '> >) (list 'debug debug))
 '((perform (op initialize-stack))
   (assign continue (label fact-done))     ; set up final return address
 fact-loop
   (test (op =) (reg n) (const 1))
   (branch (label base-case))
   ;; Set up for the recursive call by saving n and continue.
   ;; Set up continue so that the computation will continue
   ;; at after-fact when the subroutine returns.
   (save continue)
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-fact))
   (goto (label fact-loop))
 after-fact
   (restore n)
   (restore continue)
   (assign val (op *) (reg n) (reg val))   ; val now contains n(n - 1)!
   (goto (reg continue))                   ; return to caller
 base-case
   (assign val (const 1))                  ; base case: 1! = 1
   (goto (reg continue))                   ; return to caller
 fact-done
   (perform (op print-stack-statistics)))))

(define (test-5.45-machine-coded n)
  (set-register-contents! recursive-factorial-machine 'n n)
  (start recursive-factorial-machine)
  (get-register-contents recursive-factorial-machine 'val))

#|
(define (evaluated-factorial n)
  (if (= n 1)
      1
      (* (evaluated-factorial (- n 1)) n)))
|#

; A.

; interpreted 
; n = 20, (total-pushes = 624 maximum-depth = 103)
; n = 30, (total-pushes = 944 maximum-depth = 153)
; n = 40, (total-pushes = 1264 maximum-depth = 203)
; n = 50, (total-pushes = 1584 maximum-depth = 253)
; n = 1k, (total-pushes = 31984 maximum-depth = 5003)

; compiled
; n = 20 (total-pushes = 62 maximum-depth = 39)
; n = 30 (total-pushes = 92 maximum-depth = 59)
; n = 40 (total-pushes = 122 maximum-depth = 79)
; n = 50 (total-pushes = 152 maximum-depth = 99)
; n = 1k (total-pushes = 3002 maximum-depth = 1999)

; interpreted vs compiled
; (factorial 20)   total-pushes-ratio = 10.06, maximum-depth-ratio = 2.64
; (factorial 30)   total-pushes-ratio = 10.26, maximum-depth-ratio = 2.59
; (factorial 40)   total-pushes-ratio = 10.36, maximum-depth-ratio = 2.56
; (factorial 50)   total-pushes-ratio = 10.42, maximum-depth-ratio = 2.55
; (factorial 1000) total-pushes-ratio = 10.65, maximum-depth-ratio = 2.50

; machine coded
; n = 20 (total-pushes = 38 maximum-depth = 38)
; n = 30 (total-pushes = 58 maximum-depth = 58)
; n = 40 (total-pushes = 78 maximum-depth = 78)
; n = 50 (total-pushes = 98 maximum-depth = 98)
; n = 1k (total-pushes = 1998 maximum-depth = 1998)

; interpreted vs machine coded
; (factorial 20)   total-pushes-ratio = 16.42, maximum-depth-ratio = 2.71
; (factorial 30)   total-pushes-ratio = 16.27, maximum-depth-ratio = 2.63
; (factorial 40)   total-pushes-ratio = 16.20, maximum-depth-ratio = 2.60
; (factorial 50)   total-pushes-ratio = 16.16, maximum-depth-ratio = 2.58
; (factorial 1000) total-pushes-ratio = 16.00, maximum-depth-ratio = 2.50

; B.

; Can you suggest improvements to the compiler that would help it generate
; code that would come closer in performance to the hand-tailored version? 

; open coded primitives makes code almost as good as hand-tailored
; for example compiled (factorial 1000) account for:
; (total-pushes = 2003 maximum-depth = 1998)

; which makes the same maximum-depth and (total-pushes + 4) as hand-tailored

; problem why compiled code makes actually 
; (total-pushes = 3002 maximum-depth = 1999)
; is because of optimization of exercise 5.42
; to look in global environment if we fail to find
; variable in compile time environment,
; this clutters env register therefore it must be saved

;;;;;;;;;;;;;;;;;
;;; Exercise 5.46
;;;;;;;;;;;;;;;;;

(define (test-5.46-compile)
  (compile-and-go
   '(define (fib n)
      (if (< n 2)
	  n
	  (+ (fib (- n 1)) (fib (- n 2)))))))

(define fib-machine
  (make-machine
   '(n continue val)
   (list (list '+ +) (list '- -) (list '< <))
   '((perform (op initialize-stack))
     (assign continue (label fib-done))
 fib-loop
     (test (op <) (reg n) (const 2))
     (branch (label immediate-answer))
     ;; set up to compute Fib(n - 1)
     (save continue)
     (assign continue (label afterfib-n-1))
     (save n)                           ; save old value of n
     (assign n (op -) (reg n) (const 1)); clobber n to n - 1
     (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                           ; upon return, val contains Fib(n - 1)
     (restore n)
     (restore continue)
     ;; set up to compute Fib(n - 2)
     (assign n (op -) (reg n) (const 2))
     (save continue)
     (assign continue (label afterfib-n-2))
     (save val)                         ; save Fib(n - 1)
     (goto (label fib-loop))
 afterfib-n-2                           ; upon return, val contains Fib(n - 2)
     (assign n (reg val))               ; n now contains Fib(n - 2)
     (restore val)                      ; val now contains Fib(n - 1)
     (restore continue)
     (assign val                        ;  Fib(n - 1) +  Fib(n - 2)
	     (op +) (reg val) (reg n)) 
     (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
     (assign val (reg n))               ; base case:  Fib(n) = n
     (goto (reg continue))
 fib-done
     (perform (op print-stack-statistics)))))

(define (test-5.46-machine-coded n)
  (set-register-contents! fib-machine 'n n)
  (start fib-machine)
  (get-register-contents fib-machine 'val))

;  machine maximum-depth = 2 * n - 2
; compiled maximum-depth = 2 * n + 1
; evaluate maximum-depth = 5 * n + 3 (from ex 5.29)

;   machine total-pushes = total-pushes fib(n-1) + total-pushes fib(n-2) + 4
;  compiled total-pushes = total-pushes fib(n-1) + total-pushes fib(n-2) + 3
;  evaluate total-pushes = total-pushes fib(n-1) + total-pushes fib(n-2) + 40

;   machine total-pushes =  4 * fib(n+1) - 4
;  compiled total-pushes = 11 * fib(n+1) - 3
;  evaluate total-pushes = 56 * fib(n+1) - 40 (from ex 5.29)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.47
;;;;;;;;;;;;;;;;;

(define (compile-procedure-call target linkage)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
	(compound-branch (make-label 'compound-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))
	  (test (op compound-procedure?) (reg proc))
	  (branch (label ,compound-branch))))
       (parallel-instruction-sequences
	(append-instruction-sequences
	 compiled-branch
	 (compile-proc-appl target compiled-linkage))
	(parallel-instruction-sequences
	 (append-instruction-sequences
	  compound-branch
	  (compile-compound-proc-appl target compiled-linkage))
	 (append-instruction-sequences
	  primitive-branch
	  (end-with-linkage linkage
           (make-instruction-sequence '(proc argl)
				      (list target)
            `((assign ,target
		      (op apply-primitive-procedure)
		      (reg proc)
		      (reg argl))))))))
       after-call))))

(define (compile-compound-proc-appl target linkage)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
	     (save continue)
             (goto (reg compapp)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
	      (save continue)
              (goto (reg compapp))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((save continue)
	    (goto (reg compapp)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE"
                target))))

(define (test-5.47-compile)
  (compile-and-go
   '(define (fib n)
      (if (< n 2)
	  n
	  (+ (fib2 (- n 1)) (fib2 (- n 2)))))))

#|
(define (fib2 n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))
|#

;;;;;;;;;;;;;;;;;
;;; Exercise 5.48
;;;;;;;;;;;;;;;;;

; changes needed for this exercise are in "eceval-ok.scm"

; I thought that it would be cooler to have "compile-and-run" work
; on local environments too, therefore I implemented it as special form.

; For example:
#|
 (begin
   (define x 10)
   (define (f x)
     (compile-and-run '(set! x 5))
     x)
   (+ x (f x)))
|#
; should return 15, because compile-and-run should set local x

; unfortunately it returns 20, because lexical addressing code from ex 5.42
; look for variables straigt into global environment if it does not have
; lexical address (which in this case it doesn't)

; when changes of ex 5.42 are disabled above code snippet returns 15

; such compile-and-run could be implemented as a procedure, but that
; would require to introduce fourth procedure type - machine coded procedures

;;;;;;;;;;;;;;;;;
;;; Exercise 5.49
;;;;;;;;;;;;;;;;;

(define (compile-exp exp env)
  (let ((code (statements (compile exp env 'val 'return))))
    (assemble code read-compile-execute-print)))

(define read-compile-execute-print
  (make-machine-modern
   (append eceval-operations `((compile-exp ,compile-exp)))
   '((perform (op user-print) (const "\n"))
   read-compile-execute-print-loop
     (perform (op initialize-stack))
     (perform (op user-print) (const "> "))
     (assign exp (op read))
     (assign env (op get-global-environment))     
     (assign instructions (op compile-exp) (reg exp) (reg env))
     (assign continue (label after-execution))
     (goto (reg instructions))
   after-execution
     (perform (op user-print) (reg val))
     (perform (op user-print) (const "\n"))
     (goto (label read-compile-execute-print-loop)))))

; (read-compile-execute-print 'trace-on)

; (start read-compile-execute-print)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.50
;;;;;;;;;;;;;;;;;

(load "compile-eval")

; (compile-evaluator)

;;;;;;;;;;;;;;;;;
;;; Exercise 5.51
;;;;;;;;;;;;;;;;;

; "scheme.c"

;;;;;;;;;;;;;;;;;
;;; Exercise 5.52
;;;;;;;;;;;;;;;;;

; "c_compiler.scm"

