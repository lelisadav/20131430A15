; top-level-eval evaluates a form in the global environment

;;***LIST OF c****r procs*****
; caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
; cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
; cddadr cddar cdddar cddddr cdddr cddr cdr 

(define extend-env
	(lambda (syms vals env)
		(extended-env-record syms vals env)))
(define empty-env
	(lambda ()	
		(empty-env-record)))
(define top-level-eval
  (lambda (form)
    ; later we may add things that are not expressions.
    (eval-exp form init-env)))

; eval-exp is the main component of the interpreter

(define eval-exp
  (lambda (exp env)
    (cases expression exp
		[lit-exp (datum) 
			(if (and (pair? datum) (eqv? (car datum) 'quote))
				(cadr datum)
				datum)]
		[var-exp (id)
			(apply-env env id
				(lambda (x) x)
				(lambda () (apply-env global-env id
					(lambda (x) x) ;procedure to call if id is in the environment 
					(lambda (x) x))))]
						;(lambda () (begin (eopl:error 'apply-env ; procedure to call if id not in env
							;"variable not found in environment: ~s" id) (newline) (display env))))))]
		[let-exp (vars exp bodies)
			(printf "I shouldn't be here, ever!")]
		[lambda-exp (id body)
			(lambda-proc-with-env id body env)]
		[if-else-exp (test-exp then-exp else-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env)
				(eval-exp else-exp env))]
		[if-exp-null (test-exp then-exp)
			(if (eval-exp test-exp env)
				(eval-exp then-exp env))]
		[app-exp (rator rands) 
			(let* ([proc-value (eval-exp rator env)]
					[args (eval-rands rands env)])
				(apply-proc proc-value args))]
		[else (eopl:error 'eval-exp "Bad abstract syntax: ~a" exp)])))

;Gets the last element in a list.
(define last 
	(lambda (ls)
		(cond [(null? (cdr ls)) (car ls)]
			[else (last (cdr ls))])))
		
; evaluate the list of operands, putting results into a list
(define eval-rands
  (lambda (rands env)
    (map (lambda (x)
	(eval-exp x env))
		rands)))

;  Apply a procedure to its arguments.
;  At this point, we only have primitive procedures.  
;  User-defined procedures will be added later.
(define apply-proc
	(lambda (proc-value args)
		(cases proc-val proc-value
			[prim-proc (op) (apply-prim-proc op args)]
			[lambda-proc-with-env (id body envi) (apply-lambda id body args envi)]
			[else (error 'apply-proc
                "Attempt to apply bad procedure: ~s" 
                proc-value)])))
				
;Evaluates the lambda.
(define apply-lambda
	(lambda (id body args env)
		(let ([envi 
			(if (or (symbol? id) (not (list? id)))
				(with-lists id args env)
				(extend-env 
					id
					args env))])
		;change to loop
		(last (map (lambda (x) 
				(eval-exp x envi))
				body)))))
						
(define with-lists 
	(lambda (vars args env)
		(cond [(symbol? vars) 
				(extend-env (list vars) 
					(list args) env)]
			[(not (list? vars)) 
				(let* ([x-vars (get-nice-vars vars)]
						[x-args (find-correct-args args (get-list-placement vars 0) 0)])
					(extend-env x-vars x-args env))])))
					
(define get-nice-vars
	(lambda (nls)
		(cond [(not (pair? nls)) (cons nls '())]
			[else (cons (car nls) (get-nice-vars (cdr nls)))])))
			
(define get-list-placement 
	(lambda (vars count)
		(cond [(not (pair? vars)) count]
			[else (get-list-placement (cdr vars) (+ 1 count))])))
			
(define find-correct-args
	(lambda (args place count)
		(cond [(equal? count place) 
				(list args)]
			[else (cons (car args) (find-correct-args (cdr args) place (+ 1 count)))])))

(define *prim-proc-names* 
	'(+ - add1 sub1 cons = * / zero? not and or < > <= >= list null? assq eq? equal? atom? 
	length list->vector list? pair? procedure? vector->list vector make-vector vector-ref 
	vector? number? symbol? set-car! set-cdr! vector-set! display
	caaaar caaadr caaar caadar caaddr caadr caar cadaar cadadr cadar caddar 
	cadddr caddr cadr car cdaaar cdaadr cdaar cdadar cdaddr cdadr cdar cddaar 
	cddadr cddar cdddar cddddr cdddr cddr cdr))

(define init-env         ; for now, our initial global environment only contains 
	(extend-env            ; procedure names.  Recall that an environment associates
		*prim-proc-names*   ;  a value (not an expression) with an identifier.
		(map prim-proc      
			*prim-proc-names*)
		(empty-env)))

(define rep      ; "read-eval-print" loop.
  (lambda ()
    (display "--> ")
    ;; notice that we don't save changes to the environment...
    (let ([answer (top-level-eval (parse-exp (read)))])
      ;; TODO: are there answers that should display differently?
      (eopl:pretty-print answer) (newline)
      (rep))))  ; tail-recursive, so stack doesn't grow.

(define eval-one-exp
	(lambda (x) 
		(top-level-eval (syntax-expand (parse-exp x)))))

(define syntax-expand
	(lambda (datum)
		(cases expression datum
			[var-exp (id) (var-exp id)]
			[lit-exp (id) (lit-exp id)]
			[lambda-exp (id body) 
				(lambda-exp id
					(map syntax-expand body))]
			[let-exp (vars vals body)
				(app-exp (lambda-exp vars (map syntax-expand body)) (map syntax-expand vals))]
			[let*-exp (vars vals body)
				(if (null? vars)
					(syntax-expand body)
					(app-exp 
						(lambda-exp (car vars) 
							(syntax-expand 
								(let*-exp (cdr vars) (cdr vals) body))) 
						(car vals)))]
			[letrec-exp (vars vals body)
				(letrec-exp vars vals body)]
			[app-exp (rator rands)
				(app-exp (syntax-expand rator) (map syntax-expand rands))]
			[set!-exp (id body)
				(set!-exp id (syntax-expand body))]
			[if-else-exp (test success fail)
				(if-else-exp test 
					(syntax-expand success) (syntax-expand fail))]
			[if-exp-null (test success)
				(if-exp-null test (syntax-expand success))])))
			


		










