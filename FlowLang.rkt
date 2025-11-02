#lang eopl

;================================================== SCANNER Y GRAMÁTICA ==================================================

;Especificación léxica
(define scanner
  '(
    (white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (float
     (digit (arbno digit) "." digit (arbno digit)) number)
    (text
     ("\"" (arbno (not #\")) "\"")
     string)
    )
  )


;Gramática
(define grammar
  '(
    ;; PROGRAMA
    (program ((arbno sentence)) a-program)

    ;; SENTENCIAS
    (sentence (expression) expression-statement)

    (sentence ("let" identifier "=" expression ";")
              var-decl-statement)

    (sentence ("def" identifier "(" (separated-list identifier ",") ")" "{" (separated-list sentence ";") "return" expression "}")
               func-decl-statement)

    (sentence ("print" "(" expression ")")
               print-statement)
    
    ;; IDENTIFICADORES, NÚMEROS, COMPLEJOS Y PRIMITIVAS
    (expression (identifier) var-exp)
    (expression (number) lit-exp)
    (expression ("complex" "("number "," number")") complex-num)
    
    (expression ("(" expression bin-primitive expression ")")
               bin-primitive-exp)

    (expression (unary-primitive "(" expression ")")
               unary-primitive-exp)

    ;; CONDICIONALES
    (expression ("if" "(" expression ")" "{" expression "}" "else" "{" expression "}")
               conditional-exp)

    ;; BLOQUES LET
    ;(expression ("let" "{"identifier "=" expression) let-exp)
    

    ;; INVOCACIÓN DE PROCEDIMIENTOS
    (expression ("calculate" identifier "(" (separated-list expression ",") ")")
               app-exp)

    ;; PRIMITIVAS
    (bin-primitive ("+") sum-prim)
    (bin-primitive ("~") sub-prim)
    (bin-primitive ("/") div-prim)
    (bin-primitive ("*") mult-prim)
    (bin-primitive ("concat") concat-prim)

    (unary-primitive ("length") length-prim)
    (unary-primitive ("add1") add1-prim)
    (unary-primitive ("sub1") sub1-prim)
    )
  )

;=============================================== EVALUADOR DE PROGRAMA ===================================================

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (sentences) (execute-sentence-list sentences init-env)))))

(define init-env
  (lambda () empty-env))

;============================================== EJECUTOR DE SENTENCIAS ===================================================

(define execute-sentence-list
  (lambda (sentences env)
    (if (null? sentences) "Fin del programa"
        (let ((new-env (execute-sentence (car sentences) env)))
          (execute-sentence-list (cdr sentences) new-env)))))

(define execute-sentence
  (lambda (sent env)
    (cases sentence sent
      (var-decl-statement (id exp)
        (let ((val (eval-expression exp env)))
          (extend-env (list id) (list val) env)))

      (func-decl-statement (func-name param-ids body-sents return-exp)
        (let ((vec (make-vector 1))) 
          (let ((new-env (extended-env-record (list func-name) vec env)))
            (vector-set! vec 0
                         (direct-target (closure param-ids body-sents return-exp new-env)))
            new-env)))

      (print-statement (exp)
        (let ((val (eval-expression exp env)))
          (eopl:printf "~s~n" val))
        env)

      (expression-statement (exp)
        (eval-expression exp env) 
        env))))

;============================================== EVALUADOR DE EXPRESIONES =================================================

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (var-exp (id) (apply-env env id))
      (lit-exp (num) num)
      (complex-num (a b) a "+" b "i")
      
      (bin-primitive-exp (rand1 op rand2)
                         (let ((arg1 (eval-expression rand1 env))
                               (arg2 (eval-expression rand2 env)))
                           (apply-prim-bin op arg1 arg2)))
      
      (unary-primitive-exp (op exp)
                           (let ((arg (eval-expression exp)))
                             apply-prim-un op arg))
      
      (conditional-exp (test-exp true-exp false-exp)
                       (if (true-value? (eval-expression test-exp env))
                           (eval-expression true-exp env)
                           (eval-expression false-exp env)))
      
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      )))

;==================================================== FUNCIONES ==========================================================

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body-sents (list-of sentence?)) 
   (return-exp expression?)       
   (env environment?)))

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body-sents return-exp env)
        (let ((args-env (extend-env ids args env)))
          (let ((final-body-env (execute-sentence-list body-sents args-env)))
            (eval-expression return-exp final-body-env)))))))

;=================================================== EVAL RANDS ==========================================================

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

;===================================================== BOOLEANOS =========================================================

(define true-value?
  (lambda (v)
    (if (eqv? v "true") #t #f)))

;==================================================== PRIMITIVAS =========================================================

(define apply-prim-bin
  (lambda (prim arg1 arg2)
    (cases bin-primitive prim
      (sum-prim () (+ arg1 arg2))
      (sub-prim () (- arg1 arg2))
      (div-prim () (/ arg1 arg2))
      (mult-prim () (* arg1 arg2))
      (concat-prim () (string-append arg1 arg2)))))

(define apply-prim-un
  (lambda (prim arg)
    (cases unary-primitive prim
      (length-prim () (length arg))
      (add1-prim () (+ arg 1))
      (sub1-prim () (- arg 1)))))

;========================================= TIPOS DE DATOS REFERENCIA Y BLANCO ============================================

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define expval?
  (lambda (x)
    (or (number? x)
        (boolean? x)
        (null? x)
        (list? x)
        ;(dictionary? x)
        ;(prototype? x)
        (procval? x)
        )))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;====================================================== AMBIENTES ========================================================

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define empty-env  
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;============================================== AUXILIARES PARA AMBIENTES ================================================

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;======================================================= SLLGEN ==========================================================

(sllgen:make-define-datatypes scanner grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner grammar)))

(define scan&parse
  (sllgen:make-string-parser scanner grammar))

(define just-scan
  (sllgen:make-string-scanner scanner grammar))

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner
      grammar)))