#lang racket
;basic scheme interpreter ,with "let" , "and" and  "or" sentences  implemented.
;this program support this:
;(cond ((and (> 5 3) (> 6 2) (* 3 4))))
;=> 12


(require racket/mpair)
(define (mylist->mlist lst) 
  (if (null? lst)
      '()
      (if (pair? lst)
          (let ((first (car lst)))
            (if (or (mpair? first)  (pair? first))
                (mcons (mylist->mlist first)
                  (mylist->mlist (cdr lst)))
                (mcons first (mylist->mlist (cdr lst)))))
          (let ((first (mcar lst)))
            (if (or (mpair? first)  (pair? first))
                (mcons (mylist->mlist first)
                  (mylist->mlist (mcdr lst)))
                (mcons first (mylist->mlist (mcdr lst))))))))

(define (mymlist->list mlst)
  (if (null? mlst)
      '()
      (if (mpair? mlst)
          (let ((first (mcar mlst)))
            (if (or (mpair? first)  (pair? first))
                (cons (mymlist->list first)
                  (mymlist->list (mcdr mlst)))
                (cons first (mymlist->list (mcdr mlst)))))
          (let ((first (car mlst)))
            (if (or (mpair? first)  (pair? first))
                (cons (mymlist->list first)
                  (mymlist->list (cdr mlst)))
                (cons first (mymlist->list (cdr mlst))))))))
              
(define mcadr (lambda (x) (mcar (mcdr x))))
(define set-cdr! set-mcdr!)
(define set-car! set-mcar!)

(define (self-evaluating? exp)
   (cond ((number? exp) true)
         ((string? exp) true)
         (else false)))

(define (mtagged-list? exp tag) 
  (if (mpair? exp) 
      (eq? (mcar exp) tag)
      false))


(define (tagged-list? exp tag) 
  (if (pair? exp) 
      (eq? (car exp) tag)
      false))


(define (variable? exp) (symbol? exp))  

(define (quoted? exp)  (tagged-list? exp 'quote))

(define (def_struct? exp) (tagged-list? exp 'define-struct))

(define (text-of-quotation exp)  (cadr exp))

(define (assignment? exp)  (tagged-list? exp 'set!))

(define (assignment-variable exp)  (cadr exp))

(define (assignment-value exp)  (caddr exp))

(define (definition? exp)  (tagged-list? exp 'define))

(define (let? exp)  (tagged-list? exp 'let))
(define (let-body exp)  (cddr exp)) 

(define (let-clauses exp)  (cadr exp))
(define (let->combination exp)
  (cons (make-lambda (map car (let-clauses exp)) 
               (let-body exp)) (map cadr (let-clauses exp))))

(define (definition-variable exp)
  (if (variable? (cadr exp)) 
             (cadr exp)
             (caadr exp))) 


(define (definition-value exp) 
  (if (symbol? (cadr exp)) 
      (caddr exp) 
      (make-lambda (cdadr exp)             
                   (cdddr exp)))) 


(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cdddr exp))
(define (trans-parameters exp)
  (if (null? exp)
    '()
    (if (list? (car exp))
      (begin (cons (car (car exp)) (trans-parameters (cdr exp))))
      (cons (car exp) (trans-parameters (cdr exp))))))
(define (make-lambda parameters body) 
   (cons 'lambda (cons (trans-parameters parameters) (cons 'type body))))  


(define (if? exp) (tagged-list? exp 'if)) 
(define (if-predicate exp) (cadr exp)) 

(define (if-consequent exp) (caddr exp)) 

(define (if-alternative exp)
  (if (null? (cdddr exp))
             'false
             (cadddr exp))) 

(define (make-if predicate consequent alternative)
  (if (null? consequent)
      (list 'if predicate predicate alternative)
      (list 'if predicate consequent alternative)))


(define (begin? exp)
  (tagged-list? exp 'begin))


(define (begin-actions exp) (cdr exp)) 

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (make-begin seq)
  (cons 'begin seq)) 

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq)) 
        (else (make-begin seq))))


(define (application? exp) (pair? exp))  
(define (operator exp) (car exp))
(define (operands env exp) 
  ;;; (displayln exp)
  (let ((name (operator exp)))
    ;;; (displayln (symbol? name))
    ;;; (if (symbol? name)
      ;;; (displayln (lookup-variable-value (list->symbol (append (symbol->list 'NoneCrashOnNamesdjasd) (symbol->list name))) env))
      ;;; (void))
    ;;; (displayln name)
    (if (symbol? name)
      (if (lookup-variable-value (list->symbol (append (symbol->list 'NoneCrashOnNamesdjasd) (symbol->list name))) env)
        (cddr exp)
        (cdr exp))
      (cdr exp))))

(define (no-operands? ops) (null? ops)) 

(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp)) 

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause)) 

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))


(define (eval-list-get exp env)
  ; (displayln exp)
  (if (null? exp)
    '()
    (cons (eval (car exp) env) (eval-list-get (cdr exp) env))))

(define (eval-list exp env)
  (eval-list-get (cddr exp) env))

(define (is-list? exp)
  (tagged-list? exp 'list))

(define (eval exp env)
  ;;; (displayln (construction? exp env))
  ;;; (displayln (def_struct? exp))
  ;;; (displayln exp)
  ;;; (displayln env)
  ;;; (displayln (lookup-variable-value exp env))
  ;;; (displayln (pair? exp))
; (displayln (application? exp))
 ;(if (lambda? exp)
  ;  (displayln (lambda-body exp))
   ; (void))
  (cond ((self-evaluating? exp ) exp)
        ((null? exp) (void))
        ((variable? exp) (lookup-variable-value exp env))
        ((construction? exp env) (eval-construction exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and (cdr exp) env))
        ((or? exp) (eval-or (cdr exp) env))
        ((is-list? exp) (eval-list exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) 
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((format? exp) (eval-format exp env))
        ((cond? exp)  (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((def_struct? exp) (eval-struct exp env))
        ((define-template-function? exp) (eval-define-template-function exp env))
        ((define-template-struct? exp) (eval-define-template-struct exp env))
        ((template-construction? exp env) (eval-template-construction exp env))
        ((application? exp)
         (my-apply (eval (operator exp) env)  
                (list-of-values (operands env exp) env))) 
        (else
         (error "unknown expression type -- EVAL" exp))))


(define (my-apply procedure arguments)  
  ;;; (displayln "BugHere!")
  ;;; (displayln procedure)
  ;;; (displayln (compound-procedure? procedure))
  ;;; (displayln (procedure-body) procedure)
  ;;; (displayln arguments)
  (cond ((primitive-procedure? procedure)
         (apply (primitive-implementation procedure) arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment  
           (procedure-parameters procedure)
           (list->mlist arguments) 
           (procedure-enviroment procedure))))
        (else
         (error "unkonwn procedure type -- APPLY" procedure))))


(define (define-template-function? exp) (tagged-list? exp 'define-template-function))

(define (eval-define-template-function exp env)
  (define-variable! (list->symbol (append (symbol->list 'NoneCrashOnNamesdjasd) (symbol->list (caaddr exp)))) #t env)
  ;;; (displayln (list->symbol (append (symbol->list 'NoneCrashOnNamesdjasd) (symbol->list (caaddr exp)))))
  (eval (append (list 'define) (cddr exp) ) env))

(define (format? exp) (tagged-list? exp 'format))

(define (trans-number-list x)
  (if (< x 10)
    (list (integer->char (+ x 48)))
    (let ((tail (remainder x 10))
          (head (quotient x 10)))
      (append (trans-number-list head) (list (integer->char (+ tail 48)))))))

(define (number->list exp)
  (cond 
    ((not (number? exp)) (error "Error format : %d must be a integer"))
    (else (begin ;(displayln (trans-number-list exp))
                 (trans-number-list exp) ))))

(define (calc-format env res args)
  ;;; (displayln res)
  ;;; (displayln (null? res))
  ;;; (displayln args)
  (if (null? res)
    '()
    (begin 
      ;;; (display "Error")
    (let ((fir (car res)))
      ;;; (displayln fir)
      (if (eq? fir #\%)
        (cond
          ((eq? (cadr res) #\d) (append (number->list (eval (car args) env)) (calc-format env (cddr res) (cdr args))))
          ((eq? (cadr res) #\s) (append (string->list (eval (car args) env)) (calc-format env (cddr res) (cdr args))))
          (else error "Error format : After % must be %d or %s!"))
        (append (list (car res)) (calc-format env (cdr res) args)))))))

(define (eval-format exp env)
  (let ((str (cadr exp))
        (res (cddr exp)))
    (list->string (calc-format env (string->list str) res))))

(define (list-of-values exps env)  
  ;;; (displayln exps)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (and? exp)
  (tagged-list? exp 'and))

(define (eval-and operands env)
  (define (helper result ops env)
    (if (null? ops)
      result
      (let ((tmp (eval (car ops) env)))
        (if (true? tmp)
          (helper tmp (cdr ops) env)
          false))))
  (helper true operands env))


(define (or? exp)
  (tagged-list? exp 'or))

(define (eval-or operands env)
  (if (null? operands)
      false
      (if (true? (eval (car operands) env))
      true
      (eval-or (cdr operands) env))))
      
(define (eval-sequence exps env)
 ; (displayln exps)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps )env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
 (void))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env) 
    env)  
    (void))

(define (struct-name exp env)
  (cadr exp))

(define (struct-body exp env)
  (cddr exp))

(define (is-struct? val)
  (tagged-list? val 'struct))

(define (lookup-list exp A)
 ; (display (symbol? (car (car exp)))) (display "***")
 ; (displayln A)
 ; (displayln (equal? (car (car exp)) (string->symbol A)))
  (if (null? exp)
    (error "error in node-get-A")
    (let ((body (car exp)))
      (if (eq? (car body) (string->symbol A))
          (cadr body)
          (lookup-list (cdr exp) A)))))


(define (modify-list exp A v)
;  (display '~) (displayln exp)
;  (display '~) (displayln (symbol? A))
  (if (null? exp)
      (error "error in node-set-A!")
      (let ((body (car exp)))
        (if (eq? (car body)  (string->symbol A))
            (cons (list (string->symbol A) v) (cdr exp))
            (cons body (modify-list (cdr exp) A v))))))

(define (symbol->list x)
  (string->list (symbol->string x)))
 ; (displayln (string? (symbol->string x)))
 ; (displayln (list? (string->list (symbol->string x)))))

(define (list->symbol x)
  (string->symbol (list->string x)))

(define (iter-struct name exp env)
  ;;; (displayln name)
  ;;; (displayln (cddddr name))
  ;;; (displayln (caddddr name))
  ;;; (displayln exp)
  ;;; (displayln (list? name))
  (if (not (list? name))
      (error "error in iter-struct and name is not a List Type")
      (void))
  (if (null? exp)
    '()
    (let ((body (car exp)))
        (let ((val (eval (caddr body) env))
              (subname (cadr body)))
          (define-variable! (list->symbol (append name (symbol->list '-get- ) (symbol->list subname) ))
           ; (make-procedure (list 'x) (list (list->symbol (append (symbol->list (string->symbol "(") ) (symbol->list 'lookup-list) (symbol->list (string->symbol " ") ) (symbol->list 'x) (symbol->list (string->symbol " "))
            ;                                                                    (symbol->list subname) (symbol->list (string->symbol ")") )))) env) env)
            (eval (make-lambda (list 'x) (list (list 'lookup-list 'x (symbol->string subname)) )) env) env)
      ;    (displayln  (list (list->symbol (append (symbol->list (string->symbol "(") ) (symbol->list 'lookup-list) (symbol->list (string->symbol " ") ) (symbol->list 'x) (symbol->list (string->symbol " "))
      ;                                                                          (symbol->list subname) (symbol->list (string->symbol ")") )))))
;          (displayln (list->symbol (append name (symbol->list '-get- ) (symbol->list subname) )) )
          (define-variable! (list->symbol (append name (symbol->list '-set!- ) (symbol->list subname) ))
            ;(make-procedure (list 'x 'y) (list (list->symbol (append (symbol->list 'modify-list) (symbol->list (string->symbol " ") )
             ;                                                        (symbol->list 'x) (symbol->list (string->symbol " ")) (symbol->list subname)
              ;                                                       (symbol->list (string->symbol " ")) (symbol->list 'y)))) env) env)
            (eval (make-lambda (list 'x 'y) (list (list 'modify-list 'x (symbol->string subname) 'y))) env ) env)
      ;    (displayln (list (list->symbol (append (symbol->list 'modify-list) (symbol->list (string->symbol " ") )
       ;                                                              (symbol->list 'x) (symbol->list (string->symbol " ")) (symbol->list subname)
        ;                                                             (symbol->list (string->symbol " ")) (symbol->list 'y)))))
          (cons (list subname val) (iter-struct name (cdr exp) env)) ))))


(define (define-template-struct? exp) (tagged-list? exp 'define-template-struct))

(define (get-list-lambda st len exp env)
  ;;; (displayln (mcar exp))
  ;;; (displayln (symbol? (mcar exp)))
  (if (> st len)
    env
    (begin (define-variable! (mcar exp) (eval (make-lambda (list ) (list->symbol (append (symbol->list (string->symbol "'InitalFaqFaqFaqT")) 
                                            (number->list st)))) env) env)
          ;;;  (display "~~~") (displayln (lookup-variable-value 'T1 env))
          (get-list-lambda (+ st 1) len (mcdr exp) env))))


(define (iter-template-struct name exp env)
  (if (not (list? name))
      (error "error in iter-template-struct and name is not a List Type")
      (void))
  (if (null? exp)
    '()
    (let ((body (car exp)))
        (let ((val (eval (caddr body) env))
              (subname (cadr body)))
          (define-variable! (list->symbol (append name (symbol->list '-get- ) (symbol->list subname) ))
           ; (make-procedure (list 'x) (list (list->symbol (append (symbol->list (string->symbol "(") ) (symbol->list 'lookup-list) (symbol->list (string->symbol " ") ) (symbol->list 'x) (symbol->list (string->symbol " "))
            ;                                                                    (symbol->list subname) (symbol->list (string->symbol ")") )))) env) env)
            (eval (make-lambda (list 'x) (list (list 'lookup-list 'x (symbol->string subname)) )) env) env)
      ;    (displayln  (list (list->symbol (append (symbol->list (string->symbol "(") ) (symbol->list 'lookup-list) (symbol->list (string->symbol " ") ) (symbol->list 'x) (symbol->list (string->symbol " "))
      ;                                                                          (symbol->list subname) (symbol->list (string->symbol ")") )))))
;          (displayln (list->symbol (append name (symbol->list '-get- ) (symbol->list subname) )) )
          (define-variable! (list->symbol (append name (symbol->list '-set!- ) (symbol->list subname) ))
            ;(make-procedure (list 'x 'y) (list (list->symbol (append (symbol->list 'modify-list) (symbol->list (string->symbol " ") )
             ;                                                        (symbol->list 'x) (symbol->list (string->symbol " ")) (symbol->list subname)
              ;                                                       (symbol->list (string->symbol " ")) (symbol->list 'y)))) env) env)
            (eval (make-lambda (list 'x 'y) (list (list 'modify-list 'x (symbol->string subname) 'y))) env ) env)
      ;    (displayln (list (list->symbol (append (symbol->list 'modify-list) (symbol->list (string->symbol " ") )
       ;                                                              (symbol->list 'x) (symbol->list (string->symbol " ")) (symbol->list subname)
        ;                                                             (symbol->list (string->symbol " ")) (symbol->list 'y)))))
          (cons (list subname val) (iter-template-struct name (cdr exp) env)) ))))


(define (eval-define-template-struct exp env)
  (let ((name (caddr exp))
        (para (list->mlist (cadr exp)))
        (body (cdddr exp))
        (res (gen-list 1 (mlength (list->mlist (cadr exp))))))
    (let ((new-env (extend-environment
                    para res env)))
    ;;; (displayln faq-sb-env)
    (let ((init (iter-template-struct (symbol->list name) body new-env)))
      (define-variable! name (list 'template-struct init) env)
      (void)))))

(define (template-construction? exp env)
  (let ((name (car exp)))
    (let ((val (lookup-variable-value name env)))
      (is-template-struct? val))))

(define (is-template-struct? exp) (tagged-list? exp 'template-struct))



(define (eval-struct exp env)
  ;;; (displayln "defining struct ")
  ;;; (displayln exp)
  (let ((name (cadr exp))
        (body (cddr exp)))
    ;;; (displayln (string? (symbol->string name)))
    ;;; (displayln (list? (string->list (symbol->string name))))
    ;;; (displayln (list? (symbol->list name)))
    ;;; (displayln name)
    (let ((init (iter-struct (symbol->list name) body env)))
      (define-variable! name (list 'struct init) env)
      (void))))

(define (construction? exp env)
      ; (displayln "BugHere?")
  ;;; (displayln exp)
  (if (not (pair? exp))
    #f
    (let ((name (car exp)))
      ; (displayln name)
      (let ((val (lookup-variable-value name env)))
        ; (displayln val)
        (is-struct? val)))))

(define (iter-construct exp val env)
  (if (null? exp)
      val
      (let ((newval (iter-construct (cdr exp) val env))
            (body (car exp)))
        (modify-list newval (symbol->string (car body)) (eval (cadr body) env)))))



(define (re-check exp env true-env)
  ;;; (displayln exp)
  (if (null? exp)
    '()
    (let ((body (car exp)))
      ;;; (displayln (cdr body))
      ;;; (displayln env)
      ;;; (displayln (lookup-variable-value (cadr body) env))
      ;;; (displayln (primitive-procedure-names))
      ;;; (displayln (primitive-procedure-objects))
      ;;; (display "~~~") (display (lookup-variable-value (cadr body) env)) (displayln (eval (lookup-variable-value (cadr body) env) true-env))
      (if (not (false? (lookup-variable-value (cadr body) env)))
        (cons (list (car body) (eval (lookup-variable-value (cadr body) env) true-env)) (re-check (cdr exp) env true-env))
        (cons body (re-check (cdr exp) env true-env))))))




(define (gen-list st len )
  (if (> st len)
    (mlist )
     (mcons (list->symbol (append (symbol->list  'InitalFaqFaqFaqT) (number->list st))) (gen-list (+ st 1) len))))


(define (get-list st len table env)
  (if (> st len)
    (mlist )
    (begin
     (define-variable! (list->symbol (append (symbol->list  'InitalFaqFaqFaqT) (number->list st))) (list (mcar table)) env)
    (get-list (+ st 1) len (mcdr table) env))))


(define (eval-template-construction exp env)
  (let ((name (car  exp))
        (table (list->mlist (cadr  exp)))
        (body (cddr  exp)))
        ;;; (displayln name)
        ;;; (displayln table)
        ;;; (displayln (get-list 1 (mlength table)))
        ;;; (displayln body)
    (let ((new-env (get-list 1 (mlength table) table env)))

      ;;; (displayln "Faq")
      ;;; (displayln new-env)
      (iter-construct body (re-check (cadr (lookup-variable-value name env)) env env) env))))



(define (eval-construction exp env)
  (let ((name (car exp))
        (body (cdr exp)))
    ;;; (displayln exp)
    (let ((val (cadr (lookup-variable-value name env))))
      (iter-construct body val env))))

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))


(define (make-procedure parameters body env)
  ; (display "~~~")
  ; (display (trans-parameters parameters))
  ; (display "   ")
  ; (displayln body)
  (list 'procedure (trans-parameters parameters)  body env))  

(define (compound-procedure? p)  
  (tagged-list? p 'procedure))

(define (procedure-parameters p) 
  (list->mlist (cadr p))) 


(define (procedure-body p)
  (caddr p))

(define (procedure-enviroment p)
  (cadddr p))

(define (enclosing-environment env) (mcdr env)) 
(define (first-frame env) (mcar env)) 
(define the-empty-environment (mlist )) 
(define (make-frame variables values) 
  (mcons variables values)) 


(define (frame-variables frame ) (mcar frame)) 

(define (frame-values frame) (mcdr frame))  


(define (add-binding-to-frame! var val frame)  
  (set-car! frame (mcons var (mcar frame)))  
  (set-cdr! frame (mcons val (mcdr frame))))  


(define (extend-environment vars vals base-env)  
  (if (= (mlength vars) (mlength vals))
      (begin  
      (mcons (make-frame vars vals) base-env)) 
       (if (< (length vars) (length vals))
           (error "Too many arguments supplied" vars vals)
           (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
;  (display "Looking up ")
;  (displayln var)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars)) 
             (mcar vals)) 
            (else (scan (mcdr vars) (mcdr vals))))) 
    (if (eq? env the-empty-environment)
        #f
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))



(define (set-variable-value! var val env) 
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))  

             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))  
    (if (eq? env the-empty-environment)
        (error "Unbound variable --SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env) 
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars)) 

             (set-car! vals val))
            (else (scan (mcdr vars) (mcdr vals))))) 
    (scan (frame-variables frame)  
          (frame-values frame))))


(define (my-square x ) (* x x))

(define (return-zero)
  0)

(define (return-empty)
  "")

(define (return-boolean)
  #f)

(define primitive-procedures
  (mlist (mlist 'car car) 
        (mlist 'cdr cdr) 
        (mlist 'cons cons) 
        (mlist 'null? null?) 
        (mlist '+ +) 
        (mlist '* *) 
        (mlist '- -) 
        (mlist 'lookup-list lookup-list)
        (mlist 'modify-list modify-list)
        (mlist '/ /) 
        (mlist '< <) 
        (mlist '> >) 
        (mlist 'number return-zero)
        (mlist 'string return-empty)
        (mlist ''boolean return-boolean)
        (mlist '= =) 
        (mlist 'number? number?) 
        (mlist 'pair? pair?) 
        (mlist 'displayln displayln)
        (mlist 'display display)
        (mlist 'not not) 
        (mlist 'remainder remainder)
        (mlist 'length  length)
        (mlist 'sqrt  sqrt)
        (mlist 'display display)
        (mlist 'list  list)
        (mlist 'symbol? symbol?)
        (mlist 'eq? eq?)
        (mlist 'equal? equal?)
        (mlist 'cadr cadr)
        (mlist 'append append)
        (mlist 'string-replace string-replace)
        )) 

(define primitive-procedures2
  (mlist
        (mlist '* *) 
        )) 

(define (primitive-procedure-names) 
  (mmap mcar  
       primitive-procedures))  

(define (primitive-procedure-objects)
  (mmap (lambda (proc) (mlist 'primitive (mcadr proc))) 
       primitive-procedures))


(define (setup-environment ) 
  (let ((initial-env
         (extend-environment (primitive-procedure-names) 
                             (primitive-procedure-objects)
                             the-empty-environment))) 
        (define-variable! 'true true initial-env)
        (define-variable! 'false false initial-env)
        initial-env))


        
(define (primitive-procedure? proc)  
        (mtagged-list? proc 'primitive)) 


(define (primitive-implementation proc) (mcadr proc)) 

(define (driver-loop)
  (let ((input (read))) 
    (if (eq? input eof)
        (void)
        (let ((output (eval input glb-env)))
          (user-print output)
          (driver-loop)))))


             
(define (user-print object)
   (if (compound-procedure? object)
       (display (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
       (if (eq? object (void))
           object
           (begin (display object)(newline)))))

(define glb-env (setup-environment))

;;; (define A "aa")
;;; (displayln (list? (string->list A)))
;;; (displayln (string->list A))
;;; (define B (list 2 3))
;;; (displayln (list? B))

(driver-loop)


