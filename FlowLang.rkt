#lang eopl
; require racket/file
;importar file->string función

;================================================== SCANNER Y GRAMÁTICA ==================================================

;Especificación léxica
(define scanner
  '(
    (white-sp
     (whitespace) skip)
    (comment
     ("//" (arbno (not #\newline))) skip)
    (identifier
     (letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit) "." digit (arbno digit)) number)
    (text
     ("\"" (arbno (not #\")) "\"") string)
    (boolean
     ((or "true" "false" "null")) string)
    )
  )

;Gramática
(define grammar
  '(
    ;; PROGRAMA
    (program ((arbno sentence) "end") a-program)

    ;; SENTENCIAS
    (sentence (expression) expression-statement)

    (sentence ("let" (separated-list assignment ",") ";") vars-decl-statement)

    (sentence ("set" identifier "=" expression ";") assignment-statement)

    (sentence ("def" identifier "(" (separated-list identifier ",") ")" "{" (arbno sentence) "return" expression ";" "}")
               func-decl-statement)

    (sentence ("const" (separated-list assignment ",") ";")
               const-decl-statement)

    (sentence ("print" "(" expression ")" ";")
               print-statement)

    ;; ESTRUCTURAS DE CONTROL
    (sentence ("while" expression
                  "{" (arbno sentence) "}")
               while-statement)

    (sentence ("for" identifier "in" expression
                  "{" (arbno sentence) "}")
               for-statement)
    
    (sentence ("if" expression
                  "{" (arbno sentence) "}"
               "else"
                  "{" (arbno sentence) "}")
               if-statement)

    (sentence ("switch" expression "{"
                  (arbno case-clause)
                  "default" "{" (arbno sentence) "}"
                "}")
                switch-statement)

    (case-clause ("case" expression"{" (arbno sentence) "}")
                 a-case-clause)
    
    ;; ASIGNACIÓN DE VARIABLELS
    (assignment (identifier "=" expression)
                an-assignment)
    
    ;; IDENTIFICADORES, NÚMEROS, COMPLEJOS, BOOLEANOS Y NULL
    (expression (identifier) var-exp)
    (expression (number) lit-exp)
    (expression (text) text-exp)
    (expression (boolean) bool-exp)
    
    (expression ("complex" "("expression "," expression")")
                complex-num-exp)

    ;; LISTAS
    (expression ("empty")
                empty-exp)

    (expression ("append" "(" (separated-list expression ",") ")")
                append-exp)
    
    (expression ("[" (separated-list expression ",") "]") list-exp)

    (expression ("list" "(" (separated-list expression ",") ")")
                list-exp)

    (expression ("create-list" "(" expression "," expression ")")
                create-list-exp)

    (expression ("ref-list" "(" expression "," expression ")")
                ref-list-exp)

    (expression ("set-list" "(" expression "," expression "," expression ")")
                set-list-exp)

    ;; DICCIONARIOS
    (expression ("{" (separated-list expression ":" expression "," ) "}" )
                dic-exp)

    (expression ("create-dictionary" "(" (separated-list expression "," expression ",") ")")
                dic-exp)

    (expression ("ref-dictionary" "(" expression "," expression ")")
                ref-dic-exp)

    (expression ("set-dictionary" "(" expression "," expression "," expression ")")
                set-dic-exp)

    (expression ("keys" "(" expression ")")
                get-keys-exp)

    (expression ("values" "(" expression ")")
                get-vals-exp)

    ;; PROTOTIPOS
    (sentence ("prototype" identifier "=" expression)
              prototype-decl-statement)

    (sentence ("update" identifier "." identifier "=" expression ";")
              property-set-statement)

    (expression ("get" expression "." identifier)
                property-access-exp)

    (expression ("method" expression "." identifier "(" (separated-list expression ",") ")")
                method-call-exp)

    (expression ("lambda" "(" (separated-list identifier ",") ")"
                  "{" (arbno sentence) "return" expression ";" "}")
                anon-func-exp)

    (expression ("clone" "(" expression ")")
            clone-exp)

    (expression ("this")
            this-exp)

    ;; PRIMITIVAS
    (expression ("(" expression bin-primitive expression ")")
                bin-primitive-exp)

    (expression (unary-primitive "(" expression ")")
                unary-primitive-exp)

    ;; INVOCACIÓN DE PROCEDIMIENTOS
    (expression ("call" identifier "(" (separated-list expression ",") ")")
               app-exp)
    
    ;; PRIMITIVAS PARA ENTEROS Y FLOTANTES
    (bin-primitive ("+") sum-prim)
    (bin-primitive ("-") sub-prim)
    (bin-primitive ("/") div-prim)
    (bin-primitive ("*") mult-prim)
    (bin-primitive ("%") mod-prim)
    (bin-primitive ("concat") concat-prim)

    ;; PRIMITIVAS PARA BOOLEANOS
    (bin-primitive (">") greater-prim)
    (bin-primitive ("<") less-prim)
    (bin-primitive ("==") eq-prim)
    (bin-primitive (">=") greater-eq-prim)
    (bin-primitive ("<=") less-eq-prim)
    (bin-primitive ("!=") dif-prim)
    (bin-primitive ("and") and-prim)
    (bin-primitive ("or") or-prim)
    (unary-primitive ("not") neg-prim)

    ;; PRIMITIVAS PARA LISTAS
    (unary-primitive ("empty?") empty-prim)
    (unary-primitive ("list?") is-list-prim)
    (unary-primitive ("head") head-prim)
    (unary-primitive ("tail") tail-prim)

    ;; PRIMITIVAS PARA DICCIONARIOS
    (unary-primitive ("dictionary?") is-dict-prim)
    
    ;; PRIMITIVAS UNARIAS
    (unary-primitive ("length") length-prim)
    (unary-primitive ("add1") add1-prim)
    (unary-primitive ("sub1") sub1-prim)
    )
  )

;=============================================== EVALUADOR DE PROGRAMA ===================================================

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (sentences)
                 (execute-sentence-list sentences (init-env))
                 'program-terminated))))

(define init-env
  (lambda () (empty-env)))

;============================================== EJECUTOR DE SENTENCIAS ===================================================

(define execute-sentence-list
  (lambda (sentences env)
    (if (null? sentences) env
        (let ((new-env (execute-sentence (car sentences) env)))
          (execute-sentence-list (cdr sentences) new-env)))))

(define execute-sentence
  (lambda (sent env)
    (cases sentence sent

      (vars-decl-statement (assignments)
                           (let ((ids (map
                                       (lambda (a)
                                         (cases assignment a (an-assignment (id exp) id)))
                                       assignments))
                                 (vals (map
                                        (lambda (a)
                                          (cases assignment a (an-assignment (id exp)
                                                                             (eval-expression exp env))))
                                        assignments)))
                             (let ((wrapped-vals (map
                                                  (lambda (v) (direct-target v))
                                                  vals)))
                               (extend-env ids wrapped-vals env))))
      
      (const-decl-statement (assignments)
                            (let ((ids (map
                                        (lambda (a)
                                          (cases assignment a (an-assignment (id exp) id)))
                                        assignments))
                                  (vals (map
                                         (lambda (a)
                                           (cases assignment a (an-assignment (id exp)
                                                                              (eval-expression exp env))))
                                         assignments)))
                              (let ((wrapped-vals (map
                                                   (lambda (v) (const-target v))
                                                   vals)))
                                (extend-env ids wrapped-vals env))))

      (assignment-statement(id exp)
                           (let ((new-val (eval-expression exp env)))
                             (let ((ref (apply-env-ref env id)))
                               (setref! ref new-val)))
                           env)

      (if-statement (test-exp then-sents else-sents)
                    (let ((test-val (eval-expression test-exp env)))
                      (if (true-value? test-val)
                          (execute-sentence-list then-sents env)
                          (execute-sentence-list else-sents env)
                          )
                      ))

      (for-statement (id list-exp body-sents)
                     (let ((list-val (eval-expression list-exp env)))
                       (if (not (list? list-val))
                           (eopl:error 'execute-sentence 
                                       "La expresión en un 'for' debe ser una lista. Se recibió: ~s"
                                       list-val)
                           (for-loop id list-val body-sents env)
                           )))

      (while-statement (condition-exp sents)
                       (while-loop condition-exp sents env))

      (switch-statement (switch-val-exp case-clauses default-sents)
                        (let ((switch-val (eval-expression switch-val-exp env)))
                          (find-winner case-clauses default-sents switch-val env)))

      (func-decl-statement (func-name param-ids body-sents return-exp)
                           (let ((vec (make-vector 1))) 
                             (let ((new-env (extended-env-record (list func-name) vec env)))
                               (vector-set! vec 0
                                            (direct-target (closure param-ids body-sents return-exp new-env)))
                               new-env)))

      (prototype-decl-statement (id exp)
                                (let ((proto-val (eval-expression exp env)))
                                  (cases dict-val proto-val
                                    (a-dict (keys vals proto)
                                            (let ((wrapped-val (direct-target proto-val)))
                                              (extend-env (list id) (list wrapped-val) env)))
                                    (else
                                     (eopl:error 'execute-sentence
                                                 "La sentencia 'prototipo' debe usarse con un objeto {}. Se recibió: ~s"
                                                 proto-val)))))
      
      (property-set-statement (obj-id id val-exp)
                              (let* (
                                     (obj-ref (apply-env-ref env obj-id))
                                     (obj (deref obj-ref))
                                     (key id) 
                                     (new-val (eval-expression val-exp env))
                                     )
                                (if (not (dict-val? obj))
                                    (eopl:error 'execute-sentence 
                                                "Intento de 'set' en algo que no es un objeto: ~s" obj)
                                    (cases dict-val obj
                                      (a-dict (keys-list vals-list proto-link)
                                              (let* (
                                                     (key-as-string (symbol->string key))
                                                     (pos (list-find-position key-as-string keys-list))
                                                     (new-obj 
                                                      (if (number? pos)
                                                          (let ((new-vals (list-replace-at vals-list pos new-val)))
                                                            (a-dict keys-list new-vals proto-link))
                                                          (let ((new-keys (cons key-as-string keys-list))
                                                                (new-vals (cons new-val vals-list)))
                                                            (a-dict new-keys new-vals proto-link)))))
                                                (setref! obj-ref new-obj) 
                                                ))))
                                env))
      
      (print-statement (exp)
                       (let ((val (eval-expression exp env)))
                         (begin
                           (display val)
                           (newline)
                           )
                         )
                       env)

      (expression-statement (exp)
                            (eval-expression exp env) 
                            env))))

;=============================================== AUXILIAR PARA SWITCH ====================================================

(define find-winner
  (lambda (remaining-cases default-sents switch-val env)
    (cond
      ((null? remaining-cases)
       (execute-sentence-list default-sents env))
      (else
       (cases case-clause (car remaining-cases)
         (a-case-clause (case-val-exp case-sents)
                        (let ((case-val (eval-expression case-val-exp env)))
                          (if (equal? switch-val case-val)
                              (execute-sentence-list case-sents env)
                              (find-winner (cdr remaining-cases))))))))))

;================================================ AUXILIAR PARA WHILE ====================================================

(define while-loop
  (lambda (condition-exp code-block current-env)
    (if (true-value? (eval-expression condition-exp current-env))
        (let ((new-env (execute-sentence-list code-block current-env)))
          (while-loop condition-exp code-block new-env))
        current-env)))

;================================================= AUXILIAR PARA FOR =====================================================

(define for-loop
 (lambda (id values-remaining body-sents current-env)
   (cond
     ((null? values-remaining)
      current-env)
     (else
      (let* ((current-val (car values-remaining))
             (loop-env (extend-env 
                        (list id)
                        (list (direct-target current-val))
                        current-env))
             (next-env (execute-sentence-list body-sents loop-env))
             )
        (for-loop id (cdr values-remaining) body-sents next-env)
        )))))

;============================================== AUXILIAR PARA PROTOTIPOS =================================================

(define lookup-property
  (lambda (obj key)
    (cases dict-val obj
      (a-dict (keys-list vals-list proto-link)
        (let ((key-as-string (symbol->string key)))
          (let ((pos (list-find-position key-as-string keys-list)))
            (if (number? pos)
                (list-ref vals-list pos)
                (if (equal? proto-link "null")
                    "null"
                    (lookup-property proto-link key))
                ))))
      (else 
       (eopl:error "El objeto no es un diccionario 'dict': " obj))
      )))

;============================================== EVALUADOR DE EXPRESIONES =================================================

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (var-exp (id) (apply-env env id))
      (lit-exp (num) num)
      (text-exp (txt) (format-text txt))
      (bool-exp (boolean) boolean)

      (complex-num-exp (real-exp imag-exp)
                       (let ((a (eval-expression real-exp env))
                             (b (eval-expression imag-exp env)))
                         (if (and (number? a) (number? b))
                             (make-rectangular a b)
                             (eopl:error 'eval-expression "Argumentos para 'complex' deben ser números"))))

      (empty-exp () empty)

      (list-exp (elements)
                (map (lambda (elem) (eval-expression elem env))
                     elements))

      (create-list-exp (elem-exp lst-exp)
                       (let ((elem (eval-expression elem-exp env)))
                         (let ((lst (eval-expression lst-exp env)))
                           (if (list? lst)
                               (cons elem lst)
                               (eopl:error 'eval-expression
                                           "El segundo argumento en 'create-list' debe ser una lista. Recibido: ~s"
                                           lst)))))

      (append-exp (args)
                  (let ((evaluated-args (map (lambda (arg-exp) (eval-expression arg-exp env))
                                             args)))
                    (if (and map list? evaluated-args)
                        (apply append evaluated-args)
                        (eopl:error 'eval-expression 
                                    "Los argumentos de 'append' deben ser listas."))))

      (ref-list-exp (lst-exp idx-exp)
                    (let ((lst (eval-expression lst-exp env))
                          (i (eval-expression idx-exp env)))
                      (if (and (list? lst)
                               (integer? i)
                               (>= i 0)
                               (< i (length lst)))
                          (list-ref lst i)
                          "null")))

      (set-list-exp (lst-exp idx-exp val-exp)
                    (let ((lst (eval-expression lst-exp env))
                          (i (eval-expression idx-exp env))
                          (valor (eval-expression val-exp env)))
                      (if (and (list? lst)
                               (integer? i)
                               (>= i 0)
                               (< i (length lst)))
                          (list-replace-at lst i valor)
                          (eopl:error 'eval-expression
                                      "Índice inválido ~s para set-list." i))))

      (dic-exp (identifiers expressions)
               (let ((keys (map (lambda (id) (eval-expression id env)) identifiers))
                     (vals (map (lambda (exp) (eval-expression exp env)) expressions)))
                 (a-dict keys vals "null")))
      
      (ref-dic-exp (dic-exp key-exp)
                   (let ((dic (eval-expression dic-exp env))
                         (key (eval-expression key-exp env)))
                     (cases dict-val dic
                       (a-dict (keys-list vals-list proto)
                               (let ((pos (list-find-position key keys-list)))
                                 (if (number? pos)
                                     (list-ref vals-list pos)
                                     "null")))
                       (else
                        (eopl:error 'eval-expression "Intento de acceder a algo que no es un diccionario: ~s" dic))
                       )))
      
      (set-dic-exp (dic-exp key-exp val-exp)
                   (let ((dic (eval-expression dic-exp env))
                         (key (eval-expression key-exp env))
                         (new-val (eval-expression val-exp env)))
    
                     (cases dict-val dic
                       (a-dict (keys-list vals-list proto)
                               (let ((pos (list-find-position key keys-list)))
                                 (if (not (number? pos))
                                     (eopl:error 'eval-expression "Clave no encontrada en el diccionario: ~s" key)
                                     (let ((new-vals-list (list-replace-at vals-list pos new-val)))
                                       (a-dict keys-list new-vals-list proto)
                                       ))))
                       (else
                        (eopl:error 'eval-expression "Intento de 'set' en algo que no es un diccionario: ~s" dic))
                       )))

      (get-keys-exp (dic-exp)
                    (let ((dic (eval-expression dic-exp env)))
                      (cases dict-val dic
                        (a-dict (keys vals proto)
                                keys)
                        (else
                         (eopl:error 'eval-expression "Intento de 'get-keys' en algo que no es un diccionario: ~s" dic))
                        )))

      (get-vals-exp (dic-exp)
                    (let ((dic (eval-expression dic-exp env)))
                      (cases dict-val dic
                        (a-dict (keys vals proto)
                                vals)
                        (else
                         (eopl:error 'eval-expression "Intento de 'get-vals' en algo que no es un diccionario: ~s" dic))
                        )))

      (property-access-exp (obj-exp id)
                           (let ((obj (eval-expression obj-exp env)))
                             (lookup-property obj id)))

      (method-call-exp (obj-exp id args-exps)
                       (let* (
                              (obj (eval-expression obj-exp env))
                              (proc (lookup-property obj id))
                              (args (map (lambda (exp) (eval-expression exp env))
                                         args-exps))
                              )
                         (if (not (procval? proc)) 
                             (eopl:error 'eval-expression 
                                         "Intento de llamar a un no-procedimiento: ~s" id)
                             (apply-procedure proc args obj)
                             )))

      (clone-exp (proto-exp)
                 (let ((proto-obj (eval-expression proto-exp env)))
                   (if (not (dict-val? proto-obj))
                       (eopl:error 'eval-expression "Solo se pueden clonar diccionarios: ~s" proto-obj)
                       (a-dict
                        '()           
                        '()            
                        proto-obj     
                        ))))

      (this-exp ()
                (apply-env env 'this))

      (anon-func-exp (ids sents return-exp)
                     (closure ids sents return-exp env))

      (bin-primitive-exp (rand1 op rand2)
                         (let ((arg1 (eval-expression rand1 env))
                               (arg2 (eval-expression rand2 env)))
                           (apply-prim-bin arg1 op arg2)))
      
      (unary-primitive-exp (op exp)
                           (let ((arg (eval-expression exp env)))
                             (apply-prim-un op arg)))
      
      (app-exp (rator rands)
               (let ((proc (apply-env env rator))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args "null")
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
  (lambda (proc args receiver) 
    (cases procval proc
      (closure (ids body-sents return-exp env)
        (let ((args-env (extend-env
                         (cons 'this ids)
                         (cons (direct-target receiver) args)
                         env)))
          
          (let ((final-body-env (execute-sentence-list body-sents args-env)))
            (eval-expression return-exp final-body-env)))))))

;================================================== DICCIONARIOS =========================================================

(define-datatype dict-val dict-val?
  (a-dict
   (keys (list-of string?)) 
   (vals (list-of expval?))
   (proto expval?))
  )

;=================================================== EVAL-RANDS ==========================================================

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (var-exp (id)
        (let* (
               (ref (apply-env-ref env id))
               (val (deref ref))
              )
          (if (or (list? val) (dict-val? val))
              (indirect-target ref)
              (direct-target val))))
      (else
       (direct-target (eval-expression rand env))))))


;===================================================== BOOLEANOS =========================================================

(define true-value?
  (lambda (v)
    (not (or (equal? v "false")
            (equal? v 0)
            (equal? v "null")))))

;================================================= STRINGS Y LISTAS ======================================================

(define format-text
  (lambda (txt)
    (substring txt 1 (- (string-length txt) 1))
    ))

;==================================================== PRIMITIVAS =========================================================

(define apply-prim-bin
  (lambda (arg1 prim arg2)
    (cases bin-primitive prim
      (sum-prim () (+ arg1 arg2))
      (sub-prim () (- arg1 arg2))
      (div-prim () (/ arg1 arg2))
      (mult-prim () (* arg1 arg2))
      (mod-prim () (modulo arg1 arg2))
      (concat-prim () (string-append arg1 arg2))
      (greater-prim () (if (> arg1 arg2) "true" "false"))
      (less-prim () (if (< arg1 arg2) "true" "false"))
      (eq-prim () (if (equal? arg1 arg2) "true" "false"))
      (greater-eq-prim () (if (or (> arg1 arg2) (equal? arg1 arg2)) "true" "false"))
      (less-eq-prim () (if (or (< arg1 arg2) (equal? arg1 arg2)) "true" "false"))
      (dif-prim () (if (equal? arg1 arg2) "false" "true"))
      (and-prim () (if (and (true-value? arg1) (true-value? arg2)) "true" "false"))
      (or-prim () (if (or (true-value? arg1) (true-value? arg2)) "true" "false"))
      )))

(define apply-prim-un
  (lambda (prim arg)
    (cases unary-primitive prim
      (length-prim () (if (string? arg)
                          (- (string-length arg) 2)
                          (length arg)))
      (add1-prim () (+ arg 1))
      (sub1-prim () (- arg 1))
      (neg-prim () (if (not (true-value? arg)) "true" "false"))
      (empty-prim () (if (null? arg) "true" "false"))
      (is-list-prim () (list? arg))
      (head-prim () (car arg))
      (tail-prim () (cdr arg))
      (is-dict-prim() (dict-val? arg)))))

;========================================= TIPOS DE DATOS REFERENCIA Y BLANCO ============================================

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?))
  (const-target (expval expval?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define expval?
  (lambda (x)
    (or (number? x)
        (boolean? x)
        (null? x)
        (list? x)
        (string? x)
        (dict-val? x)
        (procval? x)
        )))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)
                    (const-target (v) #t)))))))

(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (const-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (const-target (expval) expval)
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

    (let ((final-ref (cases target (primitive-deref ref)
                       (direct-target (expval1) ref)
                       (indirect-target (ref1) ref1)
                       (const-target (val) ref)))) 
      (cases target (primitive-deref final-ref)
        (direct-target (val) 
         (primitive-setref! final-ref (direct-target expval)))
        (indirect-target (r)
         (primitive-setref! final-ref (direct-target expval)))
        (const-target (val)
          (eopl:error 'setref! "ERROR: No se puede mutar una constante."))
        ))))

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
    (list-index (lambda (sym1) (equal? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

(define list-replace-at
  (lambda (lst index new-val)
    (let loop ((n 0) (current-lst lst))
      (cond
        ((null? current-lst) '())
        ((= n index)
         (cons new-val (cdr current-lst)))
        (else
         (cons (car current-lst)
               (loop (+ n 1) (cdr current-lst))))))))

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

(interpretador)