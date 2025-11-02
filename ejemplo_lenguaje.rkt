#lang eopl
#|
------------------------------------------------------------------------------------------
|                                                                                        |
|                                  PROYECTO FINAL                                        |
|                                                                                        |   
------------------------------------------------------------------------------------------
                                     ESTUDIANTES:                                        |                       |
                         - Anderson Pantoja C.    - 1841610                              |
                         - LUIS GABRIEL RODRIGUEZ - 1943075                              |
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
|                                                                                        |
|                                    ESPEC. LEXICA                                       |
|                                                                                        |
------------------------------------------------------------------------------------------
|#

(require racket/match)

(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comentario
     ("#" (arbno (not #\newline))) skip)
    (string
     (#\" (arbno (not #\")) #\") string)
    (identifier
     (letter (arbno (or letter digit "_" "?" "-"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)
    (number
     (digit (arbno digit) "." digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit) "." digit (arbno digit)) number)))



#|
------------------------------------------------------------------------------------------
|                                                                                        |
|                             ESPEC. SINTACTICA (GRAMATICA)                              |
|                                                                                        |
------------------------------------------------------------------------------------------
|#

(define grammar-simple-interpreter
  '((program (expression) un-programa)

      (expression (identifier) var-ref-exp)
     (expression (number) numero-lit)
     
     (expression (string) texto-lit)      
     (expression ("True") true-exp)
     (expression ("False") false-exp)
     
     (expression ("GLOBALS" "{" expression (arbno ";" expression) "}") globals-exp)
     (expression ("PROGRAM" "{" "proc" "main" "(" ")" "{" expression (arbno expression ";") "}" "}") program-exp)
     (expression (var-decl) var-exp)
     (express-list ("list" "<" type-exp ">" identifier "=" "(" (arbno expression ",") ")") simple-exp-lista) 
     (var-decl (express-list) lista-exp)

     ;(expression (identifier "(" (separated-list expression ",") ")") func-call-exp)
     (var-decl (type-exp identifier "=" expression) dec-var-exp)
     (var-decl ("const" type-exp identifier "=" expression) const-exp) 

     (expression ("proc" identifier "=" "function" "(" (separated-list param-decl ",") ")" "{" (arbno expression ";") "}") proc-exp)
     (param-decl (type-exp identifier) param-exp)

    
     ;(expression ("make-list" "<" type-exp ">" "(" (separated-list expression ",") ")") make-list-expr)
     (primitiva-unitaria-list ("empty?") is-null-list-prim)
     (lista-primitiva ("append") append-prim)
     (primitiva-unitaria-list ("empty") null-list-prim)
     (primitiva-unitaria-list ("list?") is-lista-prim)
     (primitiva-unitaria-list ("head") car-list-prim)
     (primitiva-unitaria-list ("tail") cdr-list-prim)
 

     ;(expression (primitiva-unitaria-list "(" expression ")") primitiva-unitaria-list-exp) 
     ;(expression (lista-primitiva "(" identifier "," (separated-list expression ",") ")") lista-exp-prim)

     (expr-vector ("vector" "<" type-exp ">" identifier "=" "[" (arbno expression ",") "]") simple-expr-vector) 
     (var-decl (expr-vector) vector-exp)

  
     (primitiva-unitaria-vector ("vector?") is-vector-prim)
     ;(expression ("make-vector" "(" expression "," expression ")") make-vector-expr)
      (vector-primitive ("set-vector") set-vector-prim)
     (vector-primitive ("append-vector") append-vector-exp) 
     (vector-primitive ("ref-vector") ref-vector-prim) 
     (vector-primitive ("delete-val-vector") delete-val-vector-exp) 

     ;(expression (primitiva-unitaria-vector "(" expression ")") primitiva-unitaria-vector-exp)
     ;(expression (vector-primitive "(" identifier "," (separated-list expression ",") ")") vector-exp-prim)

     (expr-dict ("dict" "<" type-exp "," type-exp ">" identifier "=" "{" (arbno expression ":" expression ",") "}") simple-expr-dict)
     (var-decl (expr-dict) dict-exp)

     (primitiva-unitaria-dict ("dict?") is-dict-prim) 
     ;(expression ("make-dict" "{" (separated-list expression ":" expression ",") "}") make-dict-expr) 
     (dict-primitive ("ref-dict") ref-dict-prim) 
     (dict-primitive ("set-dict") set-dict-prim)  
     (dict-primitive ("append-dict") append-dict-exp)  
     (dict-primitive ("keys-dict") keys-dict-exp) 
     (dict-primitive ("values-dict") values-dict-exp) 


     ;(expression (primitiva-unitaria-dict "(" expression ")") primitiva-unitaria-dict-exp)  
     ;(expression (dict-primitive "(" identifier "," (separated-list expression ":" expression ",") ")") dict-exp-prim) 

     (expression
       (primitive "(" (separated-list expression ",")")")
           primapp-exp)
     (primitive ("+") primitiva-suma)
     (primitive ("-") primitiva-resta)
     (primitive ("*") primitiva-multi)
     (primitive ("/") primitiva-div)
     (primitive ("concat") primitiva-concat)
     (primitive ("length") primitiva-long)

     (primitive ("<") menor-exp)
     (primitive (">") mayor-exp)
     (primitive ("<=") menorIgual-exp)
     (primitive (">=") mayorIgual-exp)
     (primitive ("==") igual-exp)
     (primitive ("!=") diferente-exp)

     (primitive ("->") asignar-valor)
     (expresion (identifier "->" expression) asignar-exp)

     (expression ("if" expression "then" expression "else" expression) if-exp)
     

     (expression ("while" "(" expression ")" "{" (arbno expression ";") "}") while-exp)
     (expression ("for" "(" identifier "->" expression ";" expression ";" identifier "->" expression ")" "{" (arbno expression ";") "}") for-exp)

     (expression ("switch" "(" expression ")" "{" (arbno "case" expression ":" (arbno expression ";") ";") "default" ":" (arbno expression ";") "}") switch-exp)

     (expression ("BLOCK" "{" (arbno expression ";") "}") block-exp)

     (expression ("LOCAL" "{" (arbno var-decl ";") "}" "{" (arbno expression ";") "}") local-exp)

     (type-exp ("int") int-type-exp)
     (type-exp ("float") float-type-exp)
     (type-exp ("bool") bool-type-exp)
     (type-exp ("string") string-type-exp)
     (expression (identifier) var-ref-exp) 
     ;(type-exp ("proc") proc-type-exp)
     ;(type-exp ("vector") vector-type-exp)
     ;(type-exp ("list") list-type-exp)
)) 



#|
------------------------------------------------------------------------------------------
|                                Parser - Scanner - Interfaz                             |
------------------------------------------------------------------------------------------
|#
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))


; Construyendo datos automáticamente
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

; Definición de la función show-the-datatypes
(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))


;Función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (body)
        (eval-expression body)))))




;Ambiente inicial
(define init-env
  (lambda ()
    (empty-env)))


#|
------------------------------------------------------------------------------------------
|                                Evalucion de expresiones                                |
------------------------------------------------------------------------------------------
|#

(define eval-expression
  (lambda (exp)
    (cond
      ((symbol? exp) (apply-env env exp))
      ((pair? exp) (eval-expresion-rec exp))
      (else (cases expression exp
              (numero-lit (datum) datum)
              (texto-lit (str) str)
              (true-exp () #t)
              (false-exp () #f)
              (var-ref-exp (id)
                (apply-env env id))
              (var-exp (var-decl)
                (eval-var-decl var-decl))
              (if-exp (test-exp then-exp else-exp)
                (eval-if test-exp then-exp else-exp))
              ; Otros casos...
              (else (eopl:error "Expresión no reconocida en eval-expression: ~s" exp))
               )))))



#|
------------------------------------------------------------------------------------------
|                             auxiliares eval-expression                                 |
------------------------------------------------------------------------------------------
|#

(define eval-var-decl
  (lambda (var-decl-exp)
    (cases var-decl var-decl-exp
      (dec-var-exp (type-exp id init-exp)
        (let ((value (eval-expression init-exp)))
          (set! env (extend-env (list id) (list value) env))
          value))
      (const-exp (type-exp id init-exp)
        (let ((value (eval-expression init-exp)))
          (set! env (extend-env (list id) (list value) env))
          value))
      (lista-exp (express-list)
        (eval-lista-exp express-list))
      (dict-exp (expr-dict)
        (eval-dict-exp expr-dict))
      (else (eopl:error "Declaración de variable no reconocida: ~s" var-decl-exp)))))




; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)

(define eval-lista-exp 
  (lambda (express-list-val)
    (cases express-list express-list-val
      (simple-exp-lista (type-exp id elements)
        ; Evaluate the list elements
        (let ((values (map eval-expression elements)))
          (set! env (extend-env (list id) (list values) env))
          values))
      (else (eopl:error "Expresión de lista no reconocida: ~s" express-list-val)))))



(define eval-dict-exp
  (lambda (expr-dict-val)
    (cases expr-dict expr-dict-val
      (simple-expr-dict (_ type-exp1 type-exp2 id entries)
        ; Crear un diccionario vacío
        (let ((dict (make-dict)))
          ; Recorrer las entradas del diccionario
          (for-each (lambda (entry)
                      ; Cada entrada es una lista: (key-expr ":" value-expr)
                      (let ((key-expr (list-ref entry 0))
                            (value-expr (list-ref entry 2)))
                        (let ((key-val (eval-expression key-expr))
                              (value-val (eval-expression value-expr)))
                          (set! dict (add-a-dict dict key-val value-val)))))
                    entries)
          ; Extender el ambiente con el nuevo diccionario
          (set! env (extend-env (list id) (list dict) env))
          dict))
      (else (eopl:error "Expresión de diccionario no reconocida: ~s" expr-dict-val)))))




(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define eval-if
  (lambda (cond-exp then-exp else-exp)
    (let ((cond-val (eval-expression cond-exp)))
      (if (not (boolean? cond-val))
          (eopl:error 'eval-if "La condición de if debe ser un booleano.")
          (if cond-val
              (eval-expression then-exp)
              (eval-expression else-exp))))))



(define eval-for
  (lambda (init-exp cond-exp update-exp body-exps env)
    ;; Evaluar la inicialización
    (eval-expression init-exp env)

    (let loop ()
      (let ((test-val (eval-expression cond-exp env)))
        (if (not (boolean? test-val))
            (eopl:error 'eval-for "La condición en el ciclo for debe ser un booleano.")
            (if test-val
                (begin
                  ;; Evaluar el cuerpo del ciclo
                  (for-each (lambda (exp) (eval-expression exp env)) body-exps)

                  (eval-expression update-exp env)
    
                  (loop))
                'done))))))

(define eval-while
  (lambda (test-exp body-exps env)
    ;; Bucle para la iteración
    (let loop ()
      (let ((test-val (eval-expression test-exp env)))
        (if (not (boolean? test-val))
            (eopl:error 'eval-while "La condición en while debe ser un booleano.")
            (if test-val
                (begin

                  (for-each (lambda (exp) (eval-expression exp env)) body-exps)
                 
                  (loop))
                'done))))))


(define eval-switch
  (lambda (exp cases default-exp env)
    (let ((test-val (eval-expression exp env)))
      (let loop ((case-list cases))
        (if (null? case-list)
            (eval-expression default-exp env)
            (let ((case (car case-list)))
              (let ((case-val (eval-expression (car case) env))
                    (case-exp (cdr case)))
                (if (equal? test-val case-val)
                    (eval-expression case-exp env)

                    (loop (cdr case-list))))))))))

(define eval-empty? 
  (lambda (list-exp env)
    (let ((list-val (eval-expression list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (null? list-val)))))

(define make-list-exp
  (lambda (type elements)
    (if (not (list? elements))
        (eopl:error 'make-list-exp "Se esperaba una lista como argumento para make-list.")
        (list 'make-list type elements))))




(define eval-head
  (lambda (list-exp env)
    (let ((list-val (eval-expression list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (if (null? list-val)
              (eopl:error "La lista está vacía.")
              (car list-val))))))

(define eval-append-list
  (lambda (list1-exp list2-exp env)
    (let ((list1-val (eval-expression list1-exp env))
          (list2-val (eval-expression list2-exp env)))
      (if (not (list? list1-val)) 
          (eopl:error "El primer argumento no es una lista.")
          (if (not (list? list2-val)) 
              (eopl:error "El segundo argumento no es una lista.")
              (append list1-val list2-val))))))

(define eval-tail
  (lambda (list-exp env)
    (let ((list-val (eval-expression list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (if (null? list-val)
              (eopl:error "La lista está vacía.")
              (cdr list-val))))))

(define eval-make-list
  (lambda (type-exp list-exp env)
    (let ((list-val (eval-expression list-exp env)))
      (if (not (list? list-val))
          (eopl:error "Se esperaba una lista.")
          (list 'make-list type-exp list-val)))))



(define eval-set-vector
  (lambda (vector-exp index-exp value-exp env)
    (let ((vector-val (eval-expression vector-exp env))
          (index-val (eval-expression index-exp env))
          (value-val (eval-expression value-exp env)))
      (if (not (vector? vector-val))
          (eopl:error "Se esperaba un vector.")
          (if (not (integer? index-val))
              (eopl:error "El índice debe ser un número entero.")
              (begin
                (vector-set! vector-val index-val value-val)
                vector-val))))))

(define eval-refe-vector
  (lambda (vector-exp index-exp env)
    (let ((vector-val (eval-expression vector-exp env))
          (index-val (eval-expression index-exp env)))
      (if (not (vector? vector-val))
          (eopl:error "Se esperaba un vector.")
          (if (not (integer? index-val))
              (eopl:error "El índice debe ser un número entero.")
              (vector-ref vector-val index-val))))))



(define eval-prim-logica
  (lambda (operator expre1 expre2 env)
    (let ((value1 (eval-expression expre1 env))
          (value2 (eval-expression expre2 env)))
      (cond
        ((eq? operator '==) (if (= value1 value2) #t #f))
        ((eq? operator '!=) (if (not (= value1 value2)) #t #f))
        ((eq? operator '<) (if (< value1 value2) #t #f))
        ((eq? operator '> ) (if (> value1 value2) #t #f))
        ((eq? operator '<=) (if (<= value1 value2) #t #f))
        ((eq? operator '>=) (if (>= value1 value2) #t #f))
        (else (eopl:error "operador no existe"))))))

(define eval-primitiva-arithmetic
  (lambda (operator expre1 expre2 env)
    (let ((value1 (eval-expression expre1 env))
          (value2 (eval-expression expre2 env)))
      (cond
        ((eq? operator '+) (+ value1 value2))
        ((eq? operator '-) (- value1 value2))
        ((eq? operator '*) ( value1 value2))
        ((eq? operator '/) (/ value1 value2))
        (else (eopl:error "operador no reconocido"))))))

(define eval-make-vector
  (lambda (size-exp value-exp env)
    (let ((size-val (eval-expression size-exp env))
          (value-val (eval-expression value-exp env)))
      (if (not (integer? size-val))
          (eopl:error "el tamaño debe ser entero.")
          (make-vector size-val value-val)))))


(define eval-expresion-rec
  (lambda (exp env)
    (cond
      ((type-exp?
        (car exp)) (apply-env env exp))
      ((eq?
        (car exp) 'cons) (apply-env env exp))
      ((eq?
        (car exp) 'while) (eval-while (cadr exp) (caddr exp) env))
      ((eq?
        (car exp) 'for)
      (eval-for
       (cadr exp) (caddr exp) (cadddr exp) (cadddr (cdr (cdr (cdr exp)))) env))
      ((eq?
        (car exp) 'proc) (eval-proc-decl (cadr exp) (caddr exp) (cadddr exp) env))
      ((eq?
        (car exp) 'call) (eval-proc-call (cadr exp) (cdr exp) env))
      ((eq?
        (car exp) 'make-list) (eval-make-list (cadr exp) (caddr exp) env))
      ((eq?
        (car exp) 'make-vector) (eval-make-vector (cadr exp) (caddr exp) env))
      ((eq?
        (car exp) 'make-dict) (eval-make-dict (cadr exp) (caddr exp) env))
      ((eq?
        (car exp) 'GLOBALS) (eval-global-exp (cadr exp) (caddr exp) env))
      ((eq?
        (car exp) 'PROGRAM) (eval-proman-exp (cadr exp) (caddr exp) env))
      ((eq?
        (car exp) 'whitch) (eval-switch (cadr exp) (caddr exp) (cadddr exp) env))
      (else
       (eopl:error "estructura no reconocida")))))


(define eval-proc-decl
  (lambda (name params body exp env)
    (define proc
      (lambda args
        (let ((nuevo-env (extend-env params args env)))
          (eval-expression body nuevo-env))))
    (extend-env name proc env)))  



(define eval-proc-call
  (lambda (name args exp env)
    (define proc (apply-env env name))
    (let ((arg-values (map (lambda (arg) (eval-expression arg env)) args)))
      (apply proc arg-values))))



(define eval-global-exp
  (lambda (exp exps env)
    (let loop ((acc (eval-expression exp env))
               (exps exps))
      (if (null? exps) 
          acc
          (loop (eval-expression (car exps) 
                                 env)
                (cdr exps))))))
(define eval-proman-exp
  (lambda (rator rands env)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc)))))

;*****************************************
;***************   Definición tipos     ******************
;*****************************************

(define-datatype type type?
  (atomic-type
   (name symbol?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?))
  (any-type)
  )

;*****************************************
;*****************   Type Checker     ******************
;*****************************************

;type-of-program: <programa> -> type
; función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (un-programa (exp) (type-of-expression exp (empty-tenv))))))

;eval-expression: <expression> <enviroment> -> type
; chequea el tipo de la expresión en el ambiente de entrada
(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (numero-lit (number)
               int-type)
      (texto-lit (t)
               string-type)
      (true-exp ()
                bool-type)
      (false-exp ()

                 bool-type)
      (var-exp (id)
               (apply-tenv tenv id))
       (var-ref-exp (id)
                   (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
      (proc-exp (texps ids body)
                (type-of-proc-exp texps ids body tenv))
      (primapp-exp (prim rands)
                   (type-of-application
                    (type-of-primitive prim)
                    (types-of-expressions rands tenv)
                    prim rands exp))
      (program-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (globals-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (while-exp (rator rands) (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (for-exp (id1 ex1 id2 exp2 rator rands) (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (switch-exp (exps cases default-exp env)
                (types-of-expressions exps))
      (local-exp (vars exps)
                (types-of-expressions exps))
      (block-exp (rands)
               types-of-expressions rands )
      #|(let-exp (ids rands body)
               (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv))|#)))

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn’t match: s != ~s in%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp)
        #t)))

;type-to-external-form: <type> -> lista o simbolo
; recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type))))
      (any-type () 'any)
      )))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

;type-of-proc-exp: (list-of <type-exp>) (list-of <symbol>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión de creación de procedimiento
(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
; función auxiliar para determinar el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression ~s:"
                                  "%expected ~s%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:%~s%had rator type ~s"
                   rator (type-to-external-form rator-type))))))

;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (primitiva-suma ()
                (proc-type (list int-type int-type) int-type))
      (primitiva-resta ()
                      (proc-type (list int-type int-type) int-type))
      (primitiva-multi ()
                 (proc-type (list int-type int-type) int-type))
      (primitiva-div ()
                 (proc-type (list int-type int-type) int-type))

      (primitiva-concat ()
                 (proc-type (list string-type string-type) string-type))
      (primitiva-long ()
                 (proc-type (list string-type) int-type))

      (mayor-exp ()
                 (proc-type (list int-type int-type) bool-type))
      (menor-exp ()
                 (proc-type (list int-type int-type) bool-type))
      (mayorIgual-exp ()
                 (proc-type (list int-type int-type) bool-type))
      (menorIgual-exp ()
                 (proc-type (list int-type int-type) bool-type))
      (igual-exp ()
                 (proc-type (list int-type int-type) bool-type))
      (diferente-exp ()
                 (proc-type (list int-type int-type) bool-type))
      (asignar-valor ()
                  (proc-type (list any-type) any-type))
      #|(decr-prim ()
                 (proc-type (list int-type) int-type))
      (zero-test-prim ()
                      (proc-type (list int-type) bool-type))|#)))

;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
; función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

;type-of-primitive: (list-of <symbol>) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión let
(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
           (extend-tenv
            ids
            (types-of-expressions rands tenv)
            tenv)))
      (type-of-expression body tenv-for-body))))

;type-of-primitive: (list-of <type-exp>) (list-of <symbol>) (list-of (list-of <type-exp>)) (list-of (list-of <symbol>)) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión letrec
(define type-of-letrec-exp
  (lambda (result-texps proc-names texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-type-expressions texps))
                           texpss))
          (result-types (expand-type-expressions result-texps)))
      (let ((the-proc-types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body
               (extend-tenv proc-names the-proc-types tenv)))
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          (type-of-expression letrec-body tenv-for-body))))))

;*****************************************
;***************     Procedimientos     ******************
;*****************************************

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))
;*****************************************
;****************     Ambientes     ******************
;*****************************************

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;*****************************************
;***************  Ambientes de tipos  ******************
;*****************************************

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

(define env (init-env))

;*****************************************
;******************  Tipos  ********************
;*****************************************

(define int-type
  (atomic-type 'int))
(define bool-type
  (atomic-type 'bool))
(define string-type
  (atomic-type 'string))
(define float-type
  (atomic-type 'float))

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (string-type-exp () string-type)
      (float-type-exp () float-type)
      #|(list-type-exp (element-type) (expand-type-expression element-type))   ;; La lista tiene un tipo interno
      (vector-type-exp (element-type) (expand-type-expression element-type)) ;; El vector también
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp)))|#)))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))


;*****************************************
;****************    Funciones Auxiliares    ̈***************
;*****************************************




; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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

;------------------------------
;DICCIONARIO
;---------------------------
(define make-dict
  (lambda ()
    '())) 


(define eval-control-flow
  (lambda (exp env)
    (cond ((eq? (car exp) 'break) 'break)
          ((eq? (car exp) 'continue) 'continue)
          (else (eval-expression exp env)))))


(define div-dict
  (lambda (f base lst)
    (if (null? lst)
        base
        (div-dict f (f (car lst) base) (cdr lst))))) 


(define eval-block
  (lambda (exp-list env)
    (div-dict (lambda (exp acc)
             (if (eq? acc 'break)
                 'break
                 (let ((result (eval-expression exp env)))
                   (if (eq? result 'continue) result 'done))))
           'done exp-list)))


(define add-a-dict
  (lambda (dict key value)
    (cons (cons key value) dict)))  
(define lookup-in-dict
  (lambda (dict key)
    (define buscar-h
      (lambda (lst)
        (cond ((null? lst) (eopl:error "clave no existe"))
              ((= (car (car lst)) key) (cdr (car lst)))
              (else (buscar-h (cdr lst))))))
    (buscar-h dict)))  

(define eval-make-dict
  (lambda (key-exp value-exp env)
    (let ((key-val (eval-expression key-exp env))
          (value-val (eval-expression value-exp env)))
      (if (not (symbol? key-val))
          (eopl:error "las claves tiene que ser un simbolo.")
          (add-a-dict (make-dict) key-val value-val)))))



(interpretador)


? Valores denotados: Enteros, Flotantes, Texto, Booleanods, Procedimientos, Listas, Vectores, Diccionarios, Ejes, Vertices, Grafos no dirigidos
? Valores expresado: Enteros, Flotantes, Texto, Booleanods, Procedimientos, Listas, Vectores, Diccionarios, Ejes, Vertices, Grafos no dirigidos

!<programa>  :=  <expresion> un-programa (exp)

!<expresion> := <numero> numero-lit (num)
!            := "\""<texto> "\"" texto-lit (txt)
!            := <identificador> var-exp (id)
!            := (<expresion> <primitiva-binaria> <expresion>) primapp-bin-exp (exp1 prim-binaria exp2)
!            := <primitiva-unaria> (<expresion>) primapp-un-exp (prim-unaria exp)

!<primitiva-binaria> :=  + (primitiva-suma)
!	                   :=  ~ (primitiva-resta)
!	                   :=  / (primitiva-div)
!	                   :=  * (primitiva-multi)
!	                   :=  concat (primitiva-concat)
!                    := > (primitiva-mayor)
!                    := < (primitiva-menor)
!                    := >= (primitiva-mayor-igual)
!                    := <= (primitiva-menor-igual)
!                    := != (primitiva-diferente)
!                    := == (primitiva-comparador-igual)

!<primitiva-unaria>  :=  longitud (primitiva-longitud)
!                    :=  add1 (primitiva-add1)
!                    :=  sub1 (primitiva-sub1)
!                    :=  neg (primitiva-negacion-booleana)



;*****************************************
;****************    Especificación Léxica   ***************
;*****************************************

(define scanner-spec-simple-interpreter
'((white-sp
   (whitespace) skip) ;Espacios en blanco
  (comment
   ("%" (arbno (not #\newline))) skip) ;Comentarios
  (identifier
   (letter (arbno (or letter digit "?"))) symbol) ;Definición de identificadores
  (number
   (digit (arbno digit)) number) ;Enteros positivos
  (number
   ("-" digit (arbno digit)) number) ;Enteros negativos
  (number
   (digit (arbno digit) "." digit (arbno digit)) number) ;Flotantes positivos
  (number
   ("_" digit (arbno digit) "." digit (arbno digit)) number) ;Flotantes negativos
  (text
   ( letter (arbno (or letter digit "_" ":"))) string)) ;Definición de texto
  ) 


;Especificación Sintáctica (gramática)

(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (identifier) var-exp)
    (expression ("\"" text "\"") text-lit)
    (expression (unary-primitve "(" expression ")") primapp-un-exp)
    (expression ("(" expression binary-primitive expression ")") primapp-bin-exp)

    
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("letrec" (arbno type-exp identifier
                                 "(" (separated-list type-exp identifier ",") ")"
                                 "=" expression) "in" expression)
                letrec-exp)
    (expression ( "(" expression (arbno expression) ")") app-exp)
    
    (expression ("False") false-exp)
    (expression ("True") true-exp)
    (expression ("proc" "(" (separated-list type-exp identifier ",") ")" expression)
                proc-exp)

 
    (binary-primitive ("+") add-prim)
    (binary-primitive ("-") substract-prim)
    (binary-primitive ("*") mult-prim)
    (binary-primitive ("/") div-prim)
    (binary-primitive ("concat") conc-prim)
    (binary-primitive (">") great-prim)
    (binary-primitive ("<") less-prim)
    (binary-primitive (">=") great-eq-prim)
    (binary-primitive ("<=") less-eq-prim)
    (binary-primitive ("!=") not-eq-prim)
    (binary-primitive ("==") equal-prim)

    (unary-primitive ("length") leng-prim)
    (unary-primitive ("neg") neg-bool-prim)
    (unary-primitive ("add1") incr-prim)
    (unary-primitive ("sub1") decr-prim)
    (unary-primitive ("zero?") zero-test-prim)
    
    (type-exp ("int") int-type-exp)
    (type-exp ("float") float-type-exp)
    (type-exp ("bool") bool-type-exp)
    ;(type-exp ("proc") proc-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")")
              proc-type-exp)
    ;;;;;;;;
    ))

;*****************************************
;********       Tipos de datos para la sintaxis abstracta de la gramática      *********
;*****************************************

;Construyendo datatypes
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)))

;*****************************************
;***************    Parser, Scanner, Interfaz     **************
;*****************************************

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;El Interpretador + checker (FrontEnd + Evaluación + señal para lectura )

(define interpretador-tipos
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (aux-interpretador  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

(define aux-interpretador
  (lambda (x)
    (if (type? (type-of-program x)) (eval-program  x) 'error)))


;*****************************************
;****************    El Interprete      ****************
;*****************************************

;eval-program: <programa> -> numero
;función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))

; Ambiente inicial
;(define init-env
;  (lambda ()
;    (extend-env
;     '(x y z)
;     '(4 2 5)
;     (empty-env))))

(define init-env
  (lambda ()
    (extend-env
     '(x y z f)
     (list 4 2 5 (closure '(y) (primapp-exp (mult-prim) (cons (var-exp 'y) (cons (primapp-exp (decr-prim) (cons (var-exp 'y) '())) '())))
                      (empty-env)))
     (empty-env))))

;eval-expression: <expression> <enviroment> -> numero
; evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (text-exp (txt) txt)
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      (proc-exp (args-texps ids body)
                (closure ids body env))
      (app-exp (rator rands)
               (let ((proc (eval-expression rator env))
                     (args (eval-rands rands env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      (letrec-exp (result-texps proc-names arg-texpss idss bodies letrec-body)
                  (eval-expression letrec-body
                                   (extend-env-recursively proc-names idss bodies env)))
      (true-exp ()
                #t)
      (false-exp ()
                 #f))))

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;apply-unary-primitive: <primitiva> <expression> -> numero
(define apply-unary-primitive
  (lambda (prim arg)
    (cases unary-primitive prim
      (leng-prim () (string-length arg))
      (incr-prim () (+ arg 1))
      (decr-prim () (- arg 1))
      (zero-test-prim () (zero? arg))

      ;Implementar negacion booleana
      ;(neg-bool-prim () (true-value? (not (true-value? arg))))
      )
    ))

;apply-binary-primitive: <primitiva> <list-of-expression> -> numero
(define apply-binary-primitive
  (lambda (prim args)
    (cases binary-primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (conc-prim () (string-append (car args) (cadr args)))
      ;Colocar los apply primitive de los operadores logicos

      )
    ))

;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;*****************************************
;*****************************************



;*****************************************
;***************   Definición tipos     ******************
;*****************************************

(define-datatype type type?
  (atomic-type
   (name symbol?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?)))

;*****************************************
;*****************************************



;*****************************************
;*****************   Type Checker     ******************
;*****************************************

;type-of-program: <programa> -> type
; función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp) (type-of-expression exp (empty-tenv))))))

;eval-expression: <expression> <enviroment> -> type
; chequea el tipo de la expresión en el ambiente de entrada
(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-exp (number)
               int-type)
      (true-exp ()
                bool-type)
      (false-exp ()
                 bool-type)
      (var-exp (id)
               (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
              (let ((test-type (type-of-expression test-exp tenv))
                    (false-type (type-of-expression false-exp tenv))
                    (true-type (type-of-expression true-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type exp)
                true-type))
      (proc-exp (texps ids body)
                (type-of-proc-exp texps ids body tenv))
      (primapp-exp (prim rands)
                   (type-of-application
                    (type-of-primitive prim)
                    (types-of-expressions rands tenv)
                    prim rands exp))
      (app-exp (rator rands)
               (type-of-application
                (type-of-expression rator tenv)
                (types-of-expressions rands tenv)
                rator rands exp))
      (let-exp (ids rands body)
               (type-of-let-exp ids rands body tenv))
      (letrec-exp (result-texps proc-names texpss idss bodies letrec-body)
                  (type-of-letrec-exp result-texps proc-names texpss idss bodies
                                      letrec-body tenv)))))

;check-equal-type!: <type> <type> <expression> -> 
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn’t match: s != ~s in%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp)
        #t)))

;type-to-external-form: <type> -> lista o simbolo
; recibe un tipo y devuelve una representación del tipo facil de leer
(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                 (append
                  (arg-types-to-external-form arg-types)
                  '(->)
                  (list (type-to-external-form result-type)))))))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons
             (type-to-external-form (car types))
             (cons '*
                   (arg-types-to-external-form (cdr types))))))))

;type-of-proc-exp: (list-of <type-exp>) (list-of <symbol>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión de creación de procedimiento
(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

;type-of-application: <type> (list-of <type>) <symbol> (list-of <symbol>) <expresion> -> <type>
; función auxiliar para determinar el tipo de una expresión de aplicación
(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each
                        check-equal-type!
                        rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression ~s:"
                                  "%expected ~s%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:%~s%had rator type ~s"
                   rator (type-to-external-form rator-type))))))

;type-of-primitive: <primitive> -> <type>
; función auxiliar para determinar el tipo de una primitiva
(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      (add-prim ()
                (proc-type (list int-type int-type) int-type))
      (substract-prim ()
                      (proc-type (list int-type int-type) int-type))
      (mult-prim ()
                 (proc-type (list int-type int-type) int-type))
      (incr-prim ()
                 (proc-type (list int-type) int-type))
      (decr-prim ()
                 (proc-type (list int-type) int-type))
      (zero-test-prim ()
                      (proc-type (list int-type) bool-type)))))

;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
; función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))

;type-of-primitive: (list-of <symbol>) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión let
(define type-of-let-exp
  (lambda (ids rands body tenv)
    (let ((tenv-for-body
           (extend-tenv
            ids
            (types-of-expressions rands tenv)
            tenv)))
      (type-of-expression body tenv-for-body))))

;type-of-primitive: (list-of <type-exp>) (list-of <symbol>) (list-of (list-of <type-exp>)) (list-of (list-of <symbol>)) (list-of <expression>) <expression> <tenv> -> <type>
; función auxiliar para determinar el tipo de una expresión letrec
(define type-of-letrec-exp
  (lambda (result-texps proc-names texpss idss bodies letrec-body tenv)
    (let ((arg-typess (map (lambda (texps)
                             (expand-type-expressions texps))
                           texpss))
          (result-types (expand-type-expressions result-texps)))
      (let ((the-proc-types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body
               (extend-tenv proc-names the-proc-types tenv)))
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          (type-of-expression letrec-body tenv-for-body))))))

;*****************************************
;*****************************************



;*****************************************
;***************     Procedimientos     ******************
;*****************************************

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;*****************************************
;*****************************************


;*****************************************
;****************     Ambientes     ******************
;*****************************************

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                   (idss (list-of (list-of symbol?)))
                                   (bodies (list-of expression?))
                                   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record
     proc-names idss bodies old-env)))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'empty-env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (apply-env old-env sym))))
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (closure (list-ref idss pos)
                                                      (list-ref bodies pos)
                                                      env)
                                             (apply-env old-env sym)))))))

;*****************************************
;*****************************************


;*****************************************
;***************  Ambientes de tipos  ******************
;*****************************************

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
    (syms (list-of symbol?))
    (vals (list-of type?))
    (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)

(define apply-tenv 
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
        (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
        (let ((pos (list-find-position sym syms)))
          (if (number? pos)
            (list-ref vals pos)
            (apply-tenv env sym)))))))

;*****************************************
;*****************************************

;*****************************************
;******************  Tipos  ********************
;*****************************************

(define int-type
  (atomic-type 'int))
(define bool-type
  (atomic-type 'bool))

(define expand-type-expression
  (lambda (texp)
    (cases type-exp texp
      (int-type-exp () int-type)
      (bool-type-exp () bool-type)
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions arg-texps)
                      (expand-type-expression result-texp))))))

(define expand-type-expressions
  (lambda (texps)
    (map expand-type-expression texps)))

;*****************************************
;*****************************************



;*****************************************
;****************    Funciones Auxiliares    ̈***************
;*****************************************

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de unambiente

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