# Integrantes

Juan Sebastian Sierra - 202343656
Daniel Andrade Reyes - 202343792
Miguel Ángel Castillo - 202341975

---
## Documentación: Un Lenguaje Híbrido

Este lenguaje es un intérprete dinámico, imperativo y basado en prototipos, con fuertes capacidades funcionales. Su sintaxis es similar a la de C/JavaScript (uso de `{...}`, `while`, `if`, `def`), pero su núcleo y semántica se basan en un modelo de ambientes y clausuras de Scheme/Lisp.

### Características Clave

* **Paradigma Híbrido:** Es un lenguaje **imperativo** (soporta `set`, `while`, `for`, y mutación de objetos/listas) pero también **funcional** (soporta `lambda`, funciones como `map`, y recursión).
* **Modelo de Objetos:** Es **basado en prototipos**. No existen "clases". Los objetos se crean clonando (`clone`) un prototipo existente. La herencia se maneja recorriendo la cadena de prototipos (`proto`) para encontrar propiedades (vía `lookup-property`).
* **Tipado Dinámico:** Los valores (números, strings, listas, diccionarios) tienen tipos, pero las variables no.
* **Modelo de Mutabilidad:** Es un modelo mixto que diferencia entre reasignación y mutación:
    * **Inmutables:** Los tipos primitivos (números, strings, booleanos) son inmutables.
    * **Mutables:** Los tipos de colección (Listas y Diccionarios) son mutables *en el sitio*.
    * **Paso de Argumentos:** Los primitivos se pasan por valor. Las colecciones se pasan por **referencia** (gracias a `eval-rand`), permitiendo que una función mute el contenido de una lista u objeto que recibió como argumento.
    * **Constantes:** El lenguaje distingue entre `let` (mutable) y `const`. `const` previene la *reasignación* de la variable (`set mi_const = ...`) y también "congela" el valor (`frozen? = #t`), previniendo la *mutación* interna (`set-list(mi_const, ...)`).

---
## Diseño de Estructuras de Datos

Esta es la parte más crítica del diseño. Migramos de un modelo "funcional puro" (listas inmutables) a un modelo "híbrido mutable" (vectores y tablas hash) para ganar eficiencia y ergonomía.

### 1. Listas: De `list` a `vector`

* **Implementación:** Nuestras listas (`mut-list`) se implementan como un *struct* que "envuelve" un **vector** mutable de Racket: `(a-list (elements vector?) (frozen? boolean?))`.

* **Por qué (Eficiencia):**
    * **Modelo Antiguo (Listas):** El acceso por índice (`ref-list`) era `O(n)`. Para encontrar el 5to elemento, debíamos recorrer los 4 anteriores.
    * **Modelo Nuevo (Vectores):** El acceso por índice (`vector-ref`) y la mutación por índice (`vector-set!`) son operaciones **`O(1)`** (tiempo constante). Son instantáneas, sin importar si la lista tiene 10 o 10 millones de elementos.

* **Por qué (Facilidad de Uso):**
    * Habilita la mutación *en el sitio*. En lugar de crear una lista nueva, `set-list(mi_lista, 0, 99)` modifica `mi_lista` directamente. Esto es mucho más intuitivo para un lenguaje con sintaxis imperativa (`while`, `for`, `set`).

### 2. Diccionarios: De Listas a `hash`

Este fue el cambio más importante del intérprete.

* **Implementación:** Nuestros diccionarios (`dict-val`) envuelven una **Tabla Hash** (`hash`) mutable de Racket: `(a-dict (fields hash?) (proto expval?) (frozen? boolean?))`.

* **Por qué (Eficiencia):**
    * **Modelo Antiguo (Dos Listas):** El `get` de una propiedad (`get obj.prop`) era `O(n)`. El intérprete tenía que escanear la lista de claves hasta encontrar la correcta. Si un objeto tenía 50 propiedades, esto era muy lento.
    * **Modelo Nuevo (Tabla Hash):** La búsqueda (`hash-ref`), inserción (`hash-set!`) y eliminación (`hash-remove!`) de propiedades son **`O(1)`** en promedio. Esto es una mejora de rendimiento masiva.

* **Por qué (Facilidad de Uso):**
    * Resolvió el problema fundamental de `this`. En el modelo inmutable, un *setter* (`setMarca`) no podía modificar el objeto. Tenía que devolver un objeto nuevo y el código externo tenía que reasignar la variable (`set moto1 = method moto1.setMarca(...)`).
    * Con las *hash tables* mutables, pudimos implementar `set-property!(this, "marca", ...)`, que modifica el objeto *en el sitio*. Esto hizo que el código de prototipos fuera limpio, ergonómico y funcionara como se espera en un lenguaje orientado a objetos.

---
## Funciones Auxiliares de Racket

Para construir el intérprete, nos apoyamos en varias funciones de Racket que nos facilitaron el trabajo:

* **`display`:** La primitiva de E/S más básica. La usamos en `print-statement` para mostrar la salida al usuario.
* **`make-rectangular`:** Racket nos da manejo de números complejos "gratis". Simplemente la llamamos para implementar nuestra expresión `complex(a, b)`.
* **`sllgen` (`...:make-string-parser`, etc.):** El generador de lexers y parsers de EOPL. Es el motor que convierte el texto de tu lenguaje en los *structs* de la gramática.
* **`define-datatype` y `cases`:** El sistema de tipos algebraicos de EOPL. Es el núcleo de todo el intérprete, permitiéndonos definir nuestros *structs* (como `a-list`, `a-dict`, `closure`) y manejarlos de forma segura.
* **`vector->list` y `list->vector`:** Funciones "puente" cruciales. Como nuestros `mut-list` usan vectores, pero las primitivas de Racket (como `append`, `cdr`) funcionan mejor con listas, usamos estas funciones para convertir entre los dos formatos dentro de nuestras primitivas (`append-exp`, `tail-prim`).

## Ejemplos de la gramática mediante el uso de scan&parse 

(begin
  ;; --- Sentencias (Estructura del Programa) ---

  ; program
  (scan&parse "print(1); end")

  ; vars-decl-statement ("let")
  (scan&parse "let x = 1, y = \"hola\"; end")

  ; assignment-statement ("set")
  (scan&parse "let x = 0; set x = 10; end")

  ; func-decl-statement ("def")
  (scan&parse "def miFuncion(a, b) { return (a + b); } end")

  ; const-decl-statement ("const")
  (scan&parse "const PI = 3.14; end")

  ; print-statement ("print")
  (scan&parse "print((1 + 1)); end")

  ; while-statement ("while")
  (scan&parse "let x = 1; while (x > 0) { set x = 0; } end")

  ; for-statement ("for")
  (scan&parse "let L = [1]; for i in L { print(i); } end")

  ; if-statement ("if")
  (scan&parse "if (1 == 1) { print(\"true\"); } else { print(\"false\"); } end")

  ; switch-statement (y case-clause)
  (scan&parse "switch 2 { case 1 { print(1); } default { print(0); } } end")

  ; prototype-decl-statement ("prototype")
  (scan&parse "prototype miProto = {\"a\": 1}; end")

  ; expression-statement ("set-property!")
  (scan&parse "let d = {}; set-property(d, \"a\", 1); end")

  ; expression-statement ("method")
  (scan&parse "prototype p = {\"f\": lambda(){return 0;}}; let o = clone(p); method o.f(); end")

  ;; --- Expresiones (Valores) ---

  ; var-exp (identifier)
  (scan&parse "let x = 10; print(x); end")

  ; lit-exp (number)
  (scan&parse "print(123); end")
  (scan&parse "print(-42); end")
  (scan&parse "print(3.1416); end")

  ; text-exp (string)
  (scan&parse "print(\"Hola Mundo\"); end")

  ; bool-exp (boolean)
  (scan&parse "print(\"true\"); end")
  (scan&parse "print(\"null\"); end")

  ; complex-num-exp ("complex")
  (scan&parse "print(complex(1, 2)); end")

  ; list-exp ("[]")
  (scan&parse "print([1, \"a\", \"true\"]); end")

  ; list-exp ("list")
  (scan&parse "print(list(1, 2, 3)); end")

  ; empty-exp ("empty")
  (scan&parse "print(empty); end")

  ; append-exp ("append")
  (scan&parse "print(append([1], [2])); end")

  ; create-list-exp ("create-list")
  (scan&parse "print(create-list(1, [2, 3])); end")

  ; ref-list-exp ("ref-list")
  (scan&parse "print(ref-list([5, 6], 0)); end")

  ; set-list-exp ("set-list")
  (scan&parse "print(set-list([1, 2], 0, 99)); end")

  ; dic-exp ("{}")
  (scan&parse "print({\"a\": 1, \"b\": 2}); end")

  ; create-dictionary-exp ("create-dictionary")
  (scan&parse "print(create-dictionary(\"a\", 1, \"b\", 2)); end")

  ; ref-dic-exp ("ref-dictionary")
  (scan&parse "print(ref-dictionary({\"a\": 1}, \"a\")); end")

  ; set-dic-exp ("set-dictionary")
  (scan&parse "print(set-dictionary({\"a\": 1}, \"a\", 99)); end")

  ; get-keys-exp ("keys")
  (scan&parse "print(keys({\"a\": 1})); end")

  ; get-vals-exp ("values")
  (scan&parse "print(values({\"a\": 1})); end")

  ; anon-func-exp ("lambda")
  (scan&parse "print(lambda(x){return x;}); end")

  ; app-exp ("call")
  (scan&parse "def f(x){return x;} print(call f(1)); end")

  ; property-access-exp ("get")
  (scan&parse "prototype p = {\"a\": 10}; print(get p.a); end")

  ; clone-exp ("clone")
  (scan&parse "prototype p = {\"a\": 10}; print(clone(p)); end")

  ; this-exp ("this")
  (scan&parse "prototype p = {\"f\": lambda(){return this;}}; let o = clone(p); print(method o.f();); end")

  ; bin-primitive-exp (Aritmética)
  (scan&parse "print((10 + 2)); end")
  (scan&parse "print((10 - 2)); end")
  (scan&parse "print((10 * 2)); end")
  (scan&parse "print((10 / 2)); end")
  (scan&parse "print((10 % 3)); end")

  ; bin-primitive-exp (Strings)
  (scan&parse "print((\"hola\" concat \" mundo\")); end")

  ; bin-primitive-exp (Comparación)
  (scan&parse "print((5 > 2)); end")
  (scan&parse "print((5 < 2)); end")
  (scan&parse "print((5 == 5)); end")
  (scan&parse "print((5 >= 5)); end")
  (scan&parse "print((5 <= 4)); end")
  (scan&parse "print((5 != 4)); end")

  ; bin-primitive-exp (Lógicas)
  (scan&parse "print((\"true\" and \"false\")); end")
  (scan&parse "print((\"true\" or \"false\")); end")

  ; unary-primitive-exp (Varias)
  (scan&parse "print(not(\"true\")); end")
  (scan&parse "print(length([1, 2, 3])); end")
  (scan&parse "print(add1(10)); end")
  (scan&parse "print(sub1(10)); end")

  ; unary-primitive-exp (Listas)
  (scan&parse "print(empty?([])); end")
  (scan&parse "print(list?([1])); end")
  (scan&parse "print(head([1, 2])); end")
  (scan&parse "print(tail([1, 2])); end")

  ; unary-primitive-exp (Diccionarios)
  (scan&parse "print(dictionary?({})); end")
)
