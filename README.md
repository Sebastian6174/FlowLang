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
