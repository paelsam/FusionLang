#lang eopl

; FusionLang

; Integrantes:
; Elkin Samir Angulo Panameño
; Leonardo Cuadro Lopez

#|
? Denotation Values: int, float, string, bool, proc, list, vector, dict, graph, edges y node.
? Expresed Values: int, float, string, bool, proc, list, vector, dict, graph, edges y node.

<globals>   ::= {<expression>}*  globals(exp)

<program>   ::= <expression> program(exp) ; main function will be defined in to the grammar

<expression>
            := <int> number-lit-int(num)
            := <float> number-lit-float(num)
            := <identifier> ::= <letter> | {<letter> | 0..9}*  identifier-exp(id)
            := <string> string-exp(str)
            := <bool> ::= True | False bool-exp(b)
            := <proc> ::= proc(<identifier>*) <expression> proc-exp(p)
            := <list> ::= () | (<expression> {, <expression>}*) list-exp(l)
            := <vector> ::= [] | [<expression> {, <expression>}*] vector-exp(v)
            := <dict> ::= {} | {<expression> : <expression> {, <expression> : <expression>}*} dict-exp(d)
            := <locals> ::= { <expression> }* <body> locals-exp(l)
            := <while> :: <bool> <body> while-exp(w)
            := <if> :: = <bool> <body> if-exp(i)
            :

            ; mutable variables and constants
            ; switch case
            ; for loop
            ; data typing
            ; graph, edges and node
            ; main function in PROGRAM block

            ; define environment

<body>      ::= { <expression> {<expression>}* } body(b) ; This is not to be in to the final grammar
<letter>    ::= A..Z | a..z

|#

; Especificación léxica

(define scanner-spec-fusionlang-interpreter
  '(
    (white-space (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier ((or letter "_" "$" "/" "&" "?") (arbno (or letter digit "_" "$" "%" "&" "?"))) symbol)
    (number-int (digit (arbno digit)) number)
    (number-int ("-" digit (arbno digit)) number)
    (number-float ((arbno digit) "." (arbno digit)) number)
    (number-float ("-" (arbno digit) "." (arbno digit)) number)
    (fusion-string ("\"" (arbno (or letter digit "_" "$" "%" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%")) "\"") string)
    (fusion-string ("'"  (arbno (or letter digit "_" "$" "%" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%")) "'") string)
    ))

; Especificación sintáctica (gramática)
(define grammar-fusionlang-interpreter
  '(
    (program (block-global block-program)                             fusion-program)
    (block-global
     ("GLOBALS" "{" (arbno expression ";") "}")                       fusion-block-global)
    (block-program
     ("PROGRAM" "{" "proc" "->" "any" "main" "=" "function" "(" ")"
                "{" "return" expression "}" "}")                      fusion-block-program)

    (expression (identifier)                                          fusion-identifier-exp)
    (expression
     (type-exp identifier "=" expression)                             fusion-var-exp)
    (expression
     ("const" type-exp identifier "=" expression)                     fusion-const-exp)
    (expression
     ("@" expression "(" (separated-list expression ",") ")")         fusion-app-exp)



    (expression (fusion-string)                                       lit-string-exp)
    (expression (number-int)                                          lit-int-exp)
    (expression (number-float)                                        lit-float-exp)
    (expression ("True")                                              lit-bool-true-exp)
    (expression ("False")                                             lit-bool-false-exp)
    (expression ("function"
                 "(" (separated-list type-exp identifier ",") ")"
                 "{" "return" expression "}")                         lit-proc-exp)


    (expression
     ("(" (separated-list expression ",") ")")                        lit-list-exp)
    (expression
     ("[" (separated-list expression ",") "]")                        lit-vector-exp)
    (expression
     ("{" (separated-list expression ":" expression ",") "}")         lit-dict-exp)


    ; Primitivas
    (expression ( binary-prim expression expression )                 binary-exp)
    (binary-prim ("+")                                                binary-add-exp)
    (binary-prim ("~")                                                binary-sub-exp)
    (binary-prim ("*")                                                binary-mul-exp)
    (binary-prim ("/")                                                binary-div-exp)
    (binary-prim ("==")                                               binary-eq-exp)
    (binary-prim ("!=")                                               binary-neq-exp)
    (binary-prim ("<")                                                binary-lt-exp)
    (binary-prim ("<=")                                               binary-lte-exp)
    (binary-prim (">")                                                binary-gt-exp)
    (binary-prim (">=")                                               binary-gte-exp)
    (binary-prim ("mod")                                              binary-mod-exp)

    (expression ( unary-prim expression )                             unary-exp)
    (unary-prim ("!")                                                 unary-neg-exp)
    (unary-prim ("@add1")                                             unary-add-exp)
    (unary-prim ("@sub1")                                             unary-sub-exp)

    ; Operador de asignación
    (expression ( "->" identifier expression )                        fusion-assign-exp)


    ;? Definición de tipos de datos
    (type-exp ("int")                                                 type-int-exp)
    (type-exp ("float")                                               type-float-exp)
    (type-exp ("string")                                              type-string-exp)
    (type-exp ("bool")                                                type-bool-exp)
    (type-exp ("proc" "->" type-exp)                                  type-proc-exp)
    (type-exp ("list" "<" type-exp ">")                               type-list-exp)
    (type-exp ("vector" "<" type-exp ">")                             type-vector-exp)
    (type-exp ("dict" "<" type-exp "," type-exp ">")                  type-dict-exp)

    ;? Implementación de tipos para grafos dirigidos
    (type-exp ("graph")                                               type-graph-exp)
    (type-exp ("edges")                                               type-edges-exp)
    (type-exp ("vertices")                                            type-vertices-exp)

    ; Primitivas internas de las listas

    (unary-prim ("@empty?")                                           list-empty-bool-exp)
    (expression ("@empty")                                            list-empty-exp)
    (unary-prim ("@head")                                             list-head-exp)
    (unary-prim ("@tail")                                             list-tail-exp)
    (unary-prim ("@list-length")                                      list-length-exp)
    (binary-prim ("@make-list")                                       list-cons-exp)
    (unary-prim ("@list?")                                            list-bool-exp)
    (binary-prim ("@append")                                          list-append-exp)

    ; Primitivas internas de los vectores

    (unary-prim ("@vector?")                                          vector-bool-exp)
    (binary-prim ("@make-vector")                                     vector-make-exp)
    (binary-prim ("@ref-vector")                                      vector-ref-exp) ;? Esto recibe 2 argumentos (vector, index)
    (binary-prim ("@vector-set" "[" expression "]")                   vector-set-exp) ;? Esto recibe 2 argumentos (vector, value)
    (binary-prim ("@append-vector")                                   vector-append-exp)
    (unary-prim ("@delete-val-vector" "[" expression "]")             vector-delete-exp)
    (unary-prim ("@length-vector")                                    vector-length-exp)

    ; Primitivas internas de los diccionarios

    (unary-prim ("@dict?")                                            dict-bool-exp) ;? Esto recibe 1 argumento (dict)
    (unary-prim ("@make-dict")                                        dict-make-exp)  ;? Esto recibe 1 argumento (dict)
    (binary-prim ("@ref-dict")                                        dict-ref-exp) ;? Esto recibe 2 argumentos (dict, key)
    (binary-prim ("@set-dict")                                        dict-set-exp) ;? Esto recibe 2 argumentos (dict-to-update, dict-to-update-with)
    (binary-prim ("@append-dict")                                     dict-append-exp) ;? Esto recibe 2 argumentos (dict-to-append, dict-to-append-with)
    (unary-prim ("@keys-dict")                                        dict-keys-exp)  ;? Esto recibe 1 argumento (dict)
    (unary-prim ("@values-dict")                                      dict-values-exp) ;? Esto recibe 1 argumento (dict)

    ; Primitivas internas de los strings

    (unary-prim ("@length")                                           string-length-exp)

    ; Secuenciación
    (expression ("BLOCK" "{" expression ";"
                         (arbno expression ";") "}")                  block-exp)
    (expression
     ("LOCALS" "{" (arbno expression ";") "}"
               "{" expression ";" (arbno expression ";") "}")                        locals-exp)

    ; Ciclos y condicionales
    (expression
     ("for" "(" expression ";" expression ";" expression ")"
            "{" (arbno expression) "}")                               for-exp)
    (expression
     ("while" "(" expression ")" "{" (arbno expression) "}")          while-exp)
    (expression
     ("if" "(" expression ")" "{" expression "}"
           "else" "{" expression "}")                                 if-exp)
    (expression
     ("switch" "(" expression ")" "{"
               (arbno "case" expression ":" "{" expression "}")
               "default" ":" "{" expression "}"
               "}")                                                   switch-exp)

    ; Primitivas del lenguaje
    ( expression
      ("print" "(" (separated-list expression ",") ")")               print-exp)

    ;? Primitivas de los grafos

    (expression ("@graph" expression expression)                      graph-exp)
    (expression ("@v" "[" (separated-list identifier ",") "]")        vertices-exp)
    (expression ("@e" "[" (separated-list edge ",") "]")              edges-exp)
    (edge ("(" identifier "=>" identifier ")")                        edge-exp)

    ;? Primitivas para pasar ejes a listas
    (unary-prim ("@edges-list")                                       graph-edges-list-exp)
    (unary-prim ("@vertices-list")                                    graph-vertices-list-exp)

    (unary-prim ("@vertices")                                         graph-vertices-exp)
    (unary-prim ("@edges")                                            graph-edges-exp)
    (unary-prim ("@outgoin-neighbors" expression)                     graph-outgoing-neighbors-exp)
    (unary-prim ("@incoming-neighbors" expression)                    graph-incoming-neighbors-exp)
    ;! Esto es raro XD
    (unary-prim ("@add-edge" expression)                              graph-add-edge-exp)
    (unary-prim ("@add-vertex" expression)                            graph-add-vertex-exp)



    ))

; eval-program: Evalúa un programa FusionLang

(define eval-program
  (lambda (f-program env)
    (cases program f-program
      (fusion-program (block-global block-program)
                      ;? Primero se evalúan las variables globales
                      ;? y luego se evalúa la función principal
                      (eval-block-program block-program (eval-block-global block-global env))
                      ))))


; eval-block-global: Evalúa un bloque de variables globales
; Evalua todas las expresiones de un bloque global para luego guardarlas en
; ambiente global (ambiente anterior al almbiente vacío)

(define eval-block-global
  (lambda (f-block-global env)
    (cases block-global f-block-global
      (fusion-block-global (expressions)
                           (let* ((ids-and-exps (get-ids-and-exps expressions env))
                                  (var-ids (car (car ids-and-exps))) (var-exps (cadr (car ids-and-exps)))
                                  (const-ids (car (cadr ids-and-exps))) (const-exps (cadr (cadr ids-and-exps)))
                                  (proc-names (car (caddr ids-and-exps))) (proc-args (cadr (caddr ids-and-exps)))
                                  (proc-bodies (caddr (caddr ids-and-exps))) (const-proc-names (car (cadddr ids-and-exps)))
                                  (const-proc-args (cadr (cadddr ids-and-exps))) (const-proc-bodies (caddr (cadddr ids-and-exps)))
                                  )
                             (if (and (null? proc-names) (null? const-proc-names))
                                 (extend-env var-ids var-exps const-ids (list->vector const-exps) env)
                                 (extend-env-recursively
                                  proc-names proc-args proc-bodies
                                  const-proc-names const-proc-args const-proc-bodies
                                  (extend-env var-ids var-exps const-ids (list->vector const-exps) env))))))))

; eval-block-program: Evalúa un bloque de programa
; Evalua todas la expresiones de un bloque de programa para luego
; evaluar la última expresión que será la función principal (main)

(define eval-block-program
  (lambda (f-block-program env)
    (cases block-program f-block-program
      (fusion-block-program (expression) (eval-identifier expression env)))))

; eval-expression: Evalúa una expresión
; Evalua una expresión de FusionLang y retorna el valor resultante
; parámetros: f-expression (expresión de FusionLang); env (ambiente de evaluación)

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (fusion-identifier-exp (id) id)
      (fusion-var-exp (_type-exp _id _assigned-exp) 'f) ;? No se evalúa la asignación
      (fusion-const-exp (_type-exp _id _assigned-exp) 'f) ;? No se evalúa la asignación
      (fusion-assign-exp (id exp)
                         (if (is-const? env id)
                             (eopl:error "Variable ~s is a constant" id)
                             (begin
                               (set-ref!
                                (apply-env-ref env id)
                                (eval-identifier exp env))
                               (convert-bool-value #t))
                             ))
      (fusion-app-exp (exp args)
                      (let ((proc (eval-identifier exp env))
                            (args (eval-operators args env)))
                        (if (procval? proc)
                            (apply-procedure proc args)
                            (eopl:error "Attempt to apply non-procedure ~s" proc))
                        ))
      (lit-string-exp (str) (decode-string str))
      (lit-int-exp (int) int)
      (lit-float-exp (float) float)
      (lit-bool-true-exp () 'True)
      (lit-bool-false-exp () 'False)
      (lit-proc-exp (_types ids body) (closure ids body env))
      (lit-list-exp (expressions) (eval-operators expressions env))
      (lit-vector-exp (expressions) (list->vector (eval-operators expressions env)))
      (lit-dict-exp (keys values) (create-dictionary (eval-operators keys env) (eval-operators values env)))
      (list-empty-exp () empty)

      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))

      (switch-exp (cond-exp cases-exp bodies-exp default-exp)
                  (let*
                      ((condition (eval-identifier cond-exp env))
                       (cases (eval-operators cases-exp env))
                       (true-case (list-find-position condition cases)))
                    (if (number? true-case)
                        (eval-identifier (list-ref bodies-exp true-case) env)
                        (eval-identifier default-exp env)
                        )))
      (for-exp (init-exp cond-exp update-exp body-exp)
               (let loop ((_i (eval-expression init-exp env)))
                 (if (eval-expression cond-exp env)
                     (begin
                       (for-each (lambda (exp) (eval-expression exp env)) body-exp)
                       (loop (eval-expression update-exp env)))
                     #t
                     )))

      (while-exp (cond-exp body-exp)
                 (let loop ()
                   (if (eval-expression cond-exp env)
                       (begin
                         (for-each (lambda (exp) (eval-expression exp env)) body-exp)
                         (loop))
                       #t
                       )))

      (locals-exp (var-exps exp-body bodies)
                  ;? Evaluar las variables locales
                  (let*
                      (
                       (ids-and-exps (get-ids-and-exps var-exps env))
                       (var-ids (car (car ids-and-exps))) (var-exps (cadr (car ids-and-exps)))
                       (const-ids (car (cadr ids-and-exps)))(const-exps (cadr (cadr ids-and-exps)))
                       (proc-names (car (caddr ids-and-exps))) (proc-args (cadr (caddr ids-and-exps)))
                       (proc-bodies (caddr (caddr ids-and-exps))) (const-proc-names (car (cadddr ids-and-exps)))
                       (const-proc-args (cadr (cadddr ids-and-exps))) (const-proc-bodies (caddr (cadddr ids-and-exps)))
                       (new-env  (if (and (null? proc-names) (null? const-proc-names))
                                     (extend-env var-ids var-exps const-ids (list->vector const-exps) env)
                                     (extend-env-recursively
                                      proc-names proc-args proc-bodies
                                      const-proc-names const-proc-args const-proc-bodies
                                      (extend-env var-ids var-exps const-ids (list->vector const-exps) env)))))

                    ;? Evaluar las expresiones del cuerpo
                    (eval-identifier (block-exp exp-body bodies) new-env)))


      (block-exp (expression expressions)
                 (let loop ((acc (eval-identifier expression env)) (exps expressions))
                   (if (null? exps)
                       acc
                       (loop (eval-identifier (car exps) env) (cdr exps)))))


      (binary-exp (binary-op exp1 exp2)
                  (let ((args (eval-operators (list exp1 exp2) env)))
                    (eval-binary-prim binary-op args env)))

      (unary-exp (unary-op exp)
                 (eval-unary-prim unary-op (eval-identifier exp env) env))


      (print-exp (expressions)
                 ;? En caso de que en la lista de expresiones haya un identificador
                 ;? Aplicar el apply-env para obtener el valor de la variable
                 ;? Si el print tiene varias expresiones, se imprimen todas separadas por un espacio
                 (for-each (lambda (exp)
                             (cond
                              [(equal? #t (eval-identifier exp env)) (display "True")]
                              [(equal? #f (eval-identifier exp env)) (display "False")]
                              [else (display (eval-identifier exp env))])
                             (display " ")) expressions)
                 (newline) #t)

      ;* Graph pide 2 expresiones, vertices y aristas
      ;* Verificar que las expresiones sean de tipo vertices y aristas
      ;* Luego crear un grafo con las expresiones

      (graph-exp (vertices edges)
                 (let
                     ((vertices (eval-identifier vertices env))
                      (edges (eval-identifier edges env)))
                   (cases expression vertices
                     (vertices-exp (_vertices) (cases expression edges
                                                 (edges-exp (_edges) (graph-exp vertices edges))
                                                 (else (eopl:error "Invalid expression"))
                                                 ))
                     (else (eopl:error "Invalid expression")))))
      (edges-exp (edges) (edges-exp edges))
      (vertices-exp (vertices) (vertices-exp vertices))
      )))

(define eval-binary-prim
  (lambda (prim values env)
    (cases binary-prim prim
      (binary-add-exp () (apply-operator + string-append values))
      (binary-sub-exp () (- (car values) (cadr values)))
      (binary-mul-exp ()
                      (* (car values) (cadr values)))
      (binary-mod-exp () (modulo (car values) (cadr values)))
      (binary-div-exp () (/ (car values) (cadr values)))
      (binary-eq-exp () (apply-operator equal? string=? values))
      (binary-neq-exp () (not (equal? (car values) (cadr values))))
      (binary-lt-exp () (apply-operator < string<? values))
      (binary-lte-exp () (apply-operator <= string<=? values))
      (binary-gt-exp ()  (apply-operator > string>? values))
      (binary-gte-exp () (apply-operator >= string>=? values))

      (vector-set-exp (pos) (vector-set (car values) (eval-identifier pos env) (cadr values)))
      (vector-ref-exp () (vector-ref (car values) (cadr values)))
      (vector-append-exp () (vector-append (car values) (cadr values)))
      (vector-make-exp () (make-vector (car values) (cadr values)))

      (list-cons-exp () (map (lambda (_val) (cadr values)) (iota (car values))))
      (list-append-exp () (append (car values) (cadr values)))

      (dict-ref-exp () (ref-dict (car values) (cadr values)))
      (dict-set-exp () (dict-set (car values) (cadr values)))

      (dict-append-exp () (append-dict (car values) (cadr values)))

      (else 'b))
    ))

(define eval-unary-prim
  (lambda (prim value env)
    (cases unary-prim prim
      (unary-neg-exp ()  (not value))
      (unary-add-exp () (+ value 1))
      (unary-sub-exp () (- value 1))
      (string-length-exp () (string-length value))
      (dict-bool-exp () (eval-dict? value))
      (dict-make-exp () (if (is-a-dict? value) value (eopl:error "This is not a dict ~s" value )))
      (dict-keys-exp () (get-keys value))
      (dict-values-exp () (get-values value))
      (vector-bool-exp ()  (vector? value))
      (vector-delete-exp (pos) (vector-delete value (eval-identifier pos env)))
      (vector-length-exp () (vector-length value))
      (list-empty-bool-exp () (null? value))
      (list-head-exp () (car value))
      (list-tail-exp () (cdr value))
      (list-length-exp () (length value))
      (list-bool-exp () (list? value))

      (graph-edges-list-exp ()
        (cases expression value
          (graph-exp (_vertices edges) 
            (cases expression edges
              (edges-exp (_edges) _edges)
              (else (eopl:error "Invalid expression"))))
          (else (eopl:error "Invalid expression"))))
      
      (graph-vertices-list-exp ()
        (cases expression value
          (graph-exp (vertices _edges) 
            (cases expression vertices
              (vertices-exp (_vertices) _vertices)
              (else (eopl:error "Invalid expression"))))
          (else (eopl:error "Invalid expression"))))

      

      (graph-vertices-exp () (cases expression value
                               (graph-exp (v _edges) v)
                               (else (eopl:error "Invalid expression"))))
      (graph-edges-exp () (cases expression value
                            (graph-exp (_vertices edges) edges)
                            (else (eopl:error "Invalid expression"))))

      (graph-add-edge-exp (edge-e)
                  (add-edge value (unparse-edge-exp (eval-expression edge-e env))))
      
      (graph-add-vertex-exp (vertex)
                  (add-vertice value (eval-expression vertex env)))

      (graph-outgoing-neighbors-exp (vertex) (vecinos-salientes value (eval-expression vertex env)))
      (graph-incoming-neighbors-exp (vertex) (vecinos-entrantes value (eval-expression vertex env)))
      )))


;*********************************************************************************
; Diccionarios

(define-datatype entry entry?
  (make-entry (key scheme-value?)
              (value scheme-value?)))

(define-datatype dictionary dictionary?
  (make-dict (dict-entries (list-of entry?))))

(define scheme-value? (lambda (val) #t))

(define is-a-dict?
  (lambda (value) (if (dictionary? value) #t #f)))

; create-dictionary: Crea una lista de datatypes de dictionary
(define create-dictionary
  (lambda (keys values)
    (make-dict (map (lambda (key value) (make-entry key value)) keys values))))

(define eval-dict?
  (lambda (val)
    (if (dictionary? val) (convert-bool-value #t) (convert-bool-value #f))))

(define get-keys
  (lambda (dict)
    (cases dictionary dict
      (make-dict (entries)
                 (map (lambda (entry) (get-key entry)) entries)
                 ))))

(define get-key
  (lambda (entrie)
    (cases entry entrie
      (make-entry (key _value)
                  key
                  ))))

(define get-values
  (lambda (dict)
    (cases dictionary dict
      (make-dict (entries)
                 (map (lambda (entry) (get-value entry)) entries)
                 ))))

(define get-value
  (lambda (entrie)
    (cases entry entrie
      (make-entry (_key value)
                  value
                  ))))

;* find-entrie: Busca una entrada en un diccionario en base a una clave
(define find-entrie
  (lambda (key entries)
    (let loop ((entries entries))
      (if (null? entries)
          #f
          (cases entry (car entries)
            (make-entry (_key _value)
                        (cond
                          ((and (and (string? key) (string? _key)) (string=? key _key)) (get-value (car entries)))
                          ((eqv? key _key) (get-value (car entries)))
                          (else (loop (cdr entries))))
                        ))))))

;* ref-dict: Busca una referencia en un diccionario
(define ref-dict
  (lambda (dict key)
    (cases dictionary dict
      (make-dict (entries) (find-entrie key entries)))
    ))

(define dict-set
  (lambda (dict-to-update dict-to-update-with)
    (cases dictionary dict-to-update
      (make-dict (entries)
                 (cases dictionary dict-to-update-with
                   (make-dict (entries-with)
                              (make-dict (append entries entries-with))))))))
(define append-dict
  (lambda (dict-to-append dict-to-append-with)
    (cases dictionary dict-to-append
      (make-dict (entries)
                 (cases dictionary dict-to-append-with
                   (make-dict (entries-with)
                              (make-dict (append entries entries-with))))))))

(define string-join
  (lambda (ls sep)
    (let loop ((ls ls) (acc ""))
      (if (null? ls)
          acc
          (loop (cdr ls) (string-append acc (car ls) sep))))))

;*********************************************************************************
;* Vectores

;* vector-delete: Elimina un elemento de un vector en la posición pos
;* Primero verifica si la posición es válida, si no, lanza un error
;* Luego crea un nuevo vector con una posición menos que el vector original
;* Luego copia los elementos del vector original al nuevo vector, excepto el elemento en la posición pos
;* i -> posición del vector original
;* j -> posición del nuevo vector
(define vector-delete
  (lambda (vec pos)
    (let ((len (vector-length vec)))
      (if (or (not (number? pos)) (>= pos len))
          (eopl:error "Index out of bounds")
          (let ((new-vec (make-vector (- len 1))))
            (let loop ((i 0) (j 0))
              (if (>= i len)
                  new-vec
                  (if (= i pos)
                      (loop (+ i 1) j)
                      (begin
                        (vector-set! new-vec j (vector-ref vec i))
                        (loop (+ i 1) (+ j 1)))))))))))

;* vector-set
;* Cambia el valor de la posición pos del vector vec por el valor val.

(define vector-set
  (lambda (vec pos val)
    (vector-set! vec pos val)
    vec))

;* vector-append
;* Añade un nuevo elemento al final del vector

(define vector-append
  (lambda (vec val)
    (let ((len (vector-length vec)))
      (let ((new-vec (make-vector (+ len 1))))
        (let loop ((i 0))
          (if (= i len)
              (begin
                (vector-set! new-vec i val)
                new-vec)
              (begin
                (vector-set! new-vec i (vector-ref vec i))
                (loop (+ i 1)))))))))

;*********************************************************************************
;* Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

; set-ref!: Cambia el valor de la posición pos del vector vec por el valor val.
; si una referencia es una constante, no se puede cambiar su valor. lanzará un error.
(define set-ref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

(define deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;****************************************************************************************
;* Funciones Auxiliares

;* Funciones auxiliares para encontrar la posición de un símbolo
;* en la lista de símbolos de un ambiente

(define rib-find-position
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    ;? Si el sym es un string se usa el string=, si no se usa eqv?
    (if (string? sym)
        (list-index (lambda (sym1) (string=? sym1 sym)) los)
        (list-index (lambda (sym1) (eqv? sym1 sym)) los))))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

; iota: Genera una lista de enteros desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
          (cons next (loop (+ next 1)))))))

; decode-string: Quita las comillas de una cadena
(define decode-string
  (lambda (str)
    (substring str 1 (- (string-length str) 1))))

; eval-opetators: Evalúa una lista de expresiones
(define eval-operators
  (lambda (ops env)
    (map (lambda (op)
           (cases expression op
             (fusion-identifier-exp (id) (apply-env env id))
             (else (eval-expression op env)))) ops)))

;* get-ids-and-exp: Obtiene una lista con los identificadores y valores de las variables, constantes y funciones
;* ((ids-vars, vals-vars) (ids-const, vals-consts) (ids-procs, args-procs, bodies-procs) (ids-const-procs, args-const-procs, bodies-const-procs))
;! La función funciona, pero es muy larga y difícil de entender
;! No hacía falta hacer una lista que contenga la lista de valores de las constantes y las funciones
(define get-ids-and-exps
  (lambda (expressions env)
    (let loop (
               (exps expressions)
               (var-ids '()) (var-exps '())
               (const-ids '()) (const-exps '())
               (proc-names '()) (proc-args '())
               (proc-bodies '())
               (const-proc-names '()) (const-proc-args '())
               (const-proc-bodies '())
               )
      (if (null? exps)
          (list (list var-ids var-exps) (list const-ids const-exps)
                (list proc-names proc-args proc-bodies) (list const-proc-names const-proc-args const-proc-bodies))
          (let ((exp (car exps)))
            (cases expression exp
              (fusion-var-exp (type-exp id assigned-exp)
                              (if (is-a-proc? assigned-exp)
                                  (cases expression assigned-exp
                                    (lit-proc-exp (_types-ids _ids _body)
                                                  (loop (cdr exps)
                                                        var-ids var-exps
                                                        const-ids const-exps
                                                        (cons id proc-names) (cons _ids proc-args)
                                                        (cons _body proc-bodies) const-proc-names
                                                        const-proc-args const-proc-bodies))
                                    (else #f))
                                  (loop
                                   (cdr exps) (cons id var-ids)
                                   (cons (eval-identifier assigned-exp env) var-exps) const-ids
                                   const-exps proc-names proc-args proc-bodies
                                   const-proc-names const-proc-args const-proc-bodies)))
              (fusion-const-exp (_type-exp id assigned-exp)
                                (if (is-a-proc? assigned-exp)
                                    (cases expression assigned-exp
                                      (lit-proc-exp (_types-ids _ids _body)
                                                    (loop (cdr exps)
                                                          var-ids var-exps
                                                          const-ids const-exps
                                                          proc-names proc-args
                                                          proc-bodies (cons id const-proc-names)
                                                          (cons _ids const-proc-args) (cons _body const-proc-bodies)))
                                      (else #f))
                                    (loop
                                     (cdr exps) var-ids
                                     var-exps (cons id const-ids)
                                     (cons (eval-identifier assigned-exp env) const-exps)
                                     proc-names proc-args proc-bodies
                                     const-proc-names const-proc-args const-proc-bodies)))
              (else (eopl:error "Invalid expression, only variables and constants are allowed"))))))))

;* is-a-proc?: Verifica si un valor es un procedimiento
(define is-a-proc?
  (lambda (val)
    (cases expression val
      (lit-proc-exp (_types-ids _ids _body) #t)
      (else #f))))

;* is-const?: Verifica si una variable es constante en el ambiente
(define is-const?
  (lambda (env id)
    (cases environment env
      (empty-env-record () #f)
      (extended-env-record (_ids _vals const-ids _const-vals _env)
                           (if (list-find-position id const-ids)
                               #t
                               (is-const? _env id))))))

;* eval-identifier: Evalua la expresión, si es un identificador, lo busca en el ambiente
;* si no, evalua la expresión normalmente
(define eval-identifier
  (lambda (exp env)
    (cases expression exp
      (fusion-identifier-exp (id) (apply-env env id))
      (else (eval-expression exp env)))
    ))

;; p-append: Lista, Lista -> Lista
;; usage: (p-append l1 l2) -> Lista de elementos de l1 y l2
(define p-append
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(null? l2) l1]
      [else (cons (car l1) (p-append (cdr l1) l2 ))]
      )
    ))

;;* add-vertices : Graph, Edge -> Graph
;;* usage: (add-vertices graph edge) -> Graph
; Recibe un grafo y una arista y devuelve un nuevo grafo con los nuevos vertices añadidos
(define add-vertices
  (lambda (graph-e edge)
    (if (null? edge)
        graph-e
        (cases expression graph-e
          (graph-exp (vertices-e edges-e)
                     (cases expression vertices-e
                       (vertices-exp (vertices-list)
                                     (if (vertex-exist? (car edge) vertices-list)
                                         (add-vertices graph-e (cdr edge))
                                         (add-vertices (graph-exp (vertices-exp (p-append vertices-list (list (car edge)))) edges-e) (cdr edge))))
                       (else (eopl:error "Invalid expression"))
                       ))
          (else (eopl:error "Invalid expression"))
          ))))

;* add-vertice: Graph, Symbol -> Graph
;* Añade un vértice al grafo
(define add-vertice
  (lambda (exp vertice)
    (cases expression exp
      (graph-exp (vertices-list edges-list)
        (cases expression vertices-list
          (vertices-exp (vertices)
                        (if (vertex-exist? vertice vertices)
                            (graph-exp vertices-list edges-list)
                            (graph-exp (vertices-exp (p-append vertices (list vertice))) edges-list)))
          (else (eopl:error "Invalid expression"))))
      (else (eopl:error "Invalid expression")))))
    

;;* edge-exist? : Edge, List -> Boolean
;;* usage: (edge-exist? edge-to-add edges-list) -> Boolean
; Verifica si una arista ya existe en el conjunto de aristas
(define edge-exist?
  (lambda (edge-to-add edges-list-exp)
    (if (null? edges-list-exp)
        #f
        (cases edge (car edges-list-exp)
          (edge-exp (v1 v2)
                    (if (and (eq? v1 (car edge-to-add)) (eq? v2 (cadr edge-to-add)))
                        #t
                        (edge-exist? edge-to-add (cdr edges-list-exp))))))))

;;* add-vertices : Graph, Edge -> Graph
;;* usage: (add-vertices graph vertex) -> Graph
;; Verifica si el vertice existe en una lista de vertices
(define vertex-exist?
  (lambda (vertex vertices)
    (if (null? vertices)
        #f
        (if (equal? vertex (car vertices))
            #t
            (vertex-exist? vertex (cdr vertices))
            )
        )
    ))

;;* add-edge : graph, edge -> graph
;;* usage: (add-edge graph edge) -> graph
;; Añade una arista nueva al conjunto de aristas del grafo
;; No deben repetirse aristas
(define add-edge
  (lambda (exp edge)
    (cases expression (add-vertices exp edge)
      (graph-exp (_ edges-list-exp)
                 (cases expression edges-list-exp
                   (edges-exp (edges-list)
                              (if (edge-exist? edge edges-list)
                                  (graph-exp _ edges-list-exp)
                                  (graph-exp _ (edges-exp (p-append edges-list (list (edge-exp (car edge) (cadr edge))))))
                                  ))
                   (else (eopl:error "Invalid expression"))))
      (else (eopl:error "Invalid expression")))))


;;* vecinos-salientes : graph, Symbol -> List
;;* usage: (vecinos-salientes graph vertice) -> List
;; Obtiene los vecinos salientes de un vértice
(define vecinos-salientes
  (lambda (exp vertice)
    (cases expression exp
      (graph-exp (_ edges-list-exp)
                 (cases expression edges-list-exp
                   (edges-exp (edges-list) (vecinos-salientes-aux edges-list vertice))
                   (else  (eopl:error "Invalid expression"))))
      (else (eopl:error "Invalid expression")))))


;;* vecinos-salientes-aux : List, Symbol -> List
;;* usage: (vecinos-salientes-aux edges-list vertice) -> List
;; Función auxiliar para obtener los vecinos salientes de un vértice
(define vecinos-salientes-aux
  (lambda (edges-list vertice)
    (if (null? edges-list)
        '()
        (cases edge (car edges-list)
          (edge-exp (v1 v2)
                    (if (eq? v1 vertice)
                        (cons v2 (vecinos-salientes-aux (cdr edges-list) vertice))
                        (vecinos-salientes-aux (cdr edges-list) vertice)))
          ))))


;;* vecinos-entrantes : graph, Symbol -> List
;;* usage: (vecinos-entrantes graph vertice) -> List
;; Obtiene los vecinos entrantes de un vértice
;; Los vecinos entrantes son aquellos vértices que tienen una arista
;; que llega al vértice dado
(define vecinos-entrantes
  (lambda (exp vertice)
    (cases expression exp
      (graph-exp (_ edges-list-exp)
                 (cases expression edges-list-exp
                   (edges-exp (edges-list) (vecinos-entrantes-aux edges-list vertice))
                   (else  (eopl:error "Invalid expression"))))
      (else (eopl:error "Invalid expression")))))


;;* vecinos-entrantes-aux : List, Symbol -> List
;; Función auxiliar para obtener los vecinos entrantes
;; de un vértice
(define vecinos-entrantes-aux
  (lambda (edges-list vertice)
    (if (null? edges-list)
        '()
        (cases edge (car edges-list)
          (edge-exp (v1 v2)
                    (if (eq? v2 vertice)
                        (cons v1 (vecinos-entrantes-aux (cdr edges-list) vertice))
                        (vecinos-entrantes-aux (cdr edges-list) vertice)))
          ))))

;* apply-operator: Aplica un predicado a una pareja de valores en base al tipo de dato de los valores
;* los valores ya están evaluados
(define apply-operator
  (lambda (pred-number pred-string values)
    (cond
      [(and (number? (car values)) (number? (cadr values))) (pred-number (car values) (cadr values))]
      [(and (string? (car values)) (string? (cadr values))) (pred-string (car values) (cadr values))]
      [else (pred-number (car values) (cadr values) )])))

;* unparse-edge-exp
(define unparse-edge-exp
  (lambda (_edge)
    (cases edge _edge
      (edge-exp (v1 v2) (list v1 v2)))))


;*********************************************************************************
;* Construyendo ambiente

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (identifiers (list-of symbol?))
   (vec vector?)
   (const-identifiers (list-of symbol?))
   (const-vals vector?)
   (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (ids vals ids-const vals-const env)
    (extended-env-record ids (list->vector vals) ids-const vals-const env)))

(define extend-env-recursively
  (lambda (var-proc-names var-identifiers var-bodies const-proc-names const-identifiers const-bodies old-env)
    (let*
        ((len (length var-proc-names))
         (len-const (length const-proc-names))
         (var-vec (make-vector len))
         (const-vec (make-vector len-const))
         (env (extended-env-record var-proc-names var-vec const-proc-names const-vec old-env)))
      (for-each
       (lambda (pos ids body)
         (vector-set! var-vec pos (closure ids body env)))
       (iota len) var-identifiers var-bodies)

      (for-each
       (lambda (pos ids body)
         (vector-set! const-vec pos (closure ids body env)))
       (iota len-const) const-identifiers const-bodies)

      env)))

(define apply-env
  (lambda (env id)
    (deref (apply-env-ref env id))))

(define apply-env-ref
  (lambda (env id)
    (cases environment env
      (empty-env-record ()
                        (eopl:error "Variable ~s is not defined" id))
      (extended-env-record (identifiers values const-identifiers const-values env)
                           (let ((pos (rib-find-position id identifiers)))
                             (if (number? pos)
                                 (a-ref pos values)
                                 (let ((const-pos (rib-find-position id const-identifiers)))
                                   (if (number? const-pos)
                                       (a-ref const-pos const-values)
                                       (apply-env-ref env id)))))))))

;*********************************************************************************
;* Definición de ambiente de tipos

(define-datatype type-environment type-environment?
  (empty-tenv-record)
  (extended-tenv-record
   (syms (list-of symbol?))
   (vals (list-of type?))
   (tenv type-environment?))
  (extended-tenv-record-rec
   (syms (list-of symbol?))
   (args-types (list-of (list-of type?)))
   (results-types (list-of type?))
   (tenv type-environment?)))

(define empty-tenv empty-tenv-record)
(define extend-tenv extended-tenv-record)
(define extend-tenv-rec extended-tenv-record-rec)

;* apply-tenv: Aplica un identificador a un ambiente de tipos
(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record ()
                         (eopl:error 'apply-tenv "Unbound variable ~s" sym))
      (extended-tenv-record (syms vals env)
                            (let ((pos (list-find-position sym syms)))
                              (if (number? pos)
                                  (list-ref vals pos)
                                  (apply-tenv env sym))))
      (extended-tenv-record-rec (syms args-types results-types env)
                                (let ((pos (list-find-position sym syms)))
                                  (if (number? pos)
                                      (proc-type (list-ref args-types pos) (list-ref results-types pos))
                                      (apply-tenv env sym)))))))


;*********************************************************************************
;* Procedimientos

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-identifier body (extend-env ids args '() (make-vector 0) env))))))


;*********************************************************************************
;* Booleanos

(define convert-bool-value
  (lambda (v)
    (cond
      [(equal? v #t) 'True]
      [(equal? v #f) 'False]
      [(equal? v 'True) #t]
      [(equal? v 'False) #f])))

;*********************************************************************************
;* Contruyendo datatypes
(sllgen:make-define-datatypes scanner-spec-fusionlang-interpreter grammar-fusionlang-interpreter)

(define show-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec-fusionlang-interpreter grammar-fusionlang-interpreter)))

;* Parser, Scanner e Interfaz
(define scan-and-parse
  (sllgen:make-string-parser scanner-spec-fusionlang-interpreter grammar-fusionlang-interpreter))

(define scanner
  (sllgen:make-string-scanner scanner-spec-fusionlang-interpreter grammar-fusionlang-interpreter))

(define interpreter
  (sllgen:make-rep-loop "Ƒ> "
                        (lambda (program) (eval-program program (empty-env)))
                        (sllgen:make-stream-parser
                         scanner-spec-fusionlang-interpreter
                         grammar-fusionlang-interpreter)))

(define type-interpreter
  (sllgen:make-rep-loop "Ƒ> "
                        (lambda (program) (aux-interpreter program))
                        (sllgen:make-stream-parser
                         scanner-spec-fusionlang-interpreter
                         grammar-fusionlang-interpreter)))

(define aux-interpreter
  (lambda (prm)
    (if (type? (type-of-program prm))
        (eval-program prm (empty-env))
        "Error de tipos en el programa")))

;***********************************************************************************
;* Definción de tipos

(define-datatype type type?
  (atomic-type
   (name symbol?))
  (list-type
   (elem-type type?))
  (vector-type
   (elem-type type?))
  (dict-type
   (key-type type?)
   (value-type type?))
  (proc-type
   (arg-types (list-of type?))
   (result-type type?))
  (graph-type
   (vertices-type type?)
   (edges-type type?)))

(define int-type (atomic-type 'int))
(define float-type (atomic-type 'float))
(define string-type (atomic-type 'string))
(define bool-type (atomic-type 'bool))
(define proc-tp (atomic-type 'proc))
(define vertices-type (atomic-type 'vertices))
(define edges-type (atomic-type 'edges))
(define edge-type (atomic-type 'edge))
(define void-type (atomic-type 'void))

;***********************************************************************************************************************
;* Type-checker

;* type-of-program: <programa> -> type
;* función que chequea el tipo de un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define type-of-program
  (lambda (f-program)
    (cases program f-program
      (fusion-program (b-global b-program)
                      (type-of-block-program b-program (type-of-block-global b-global))))))

;* type-of-block-global: <bloque-global> -> type-environment
;* función que chequea el tipo de un bloque global
(define type-of-block-global
  (lambda (f-block-global)
    (cases block-global f-block-global
      (fusion-block-global (expressions)
                           ;* Llamar a get-ids-and-exps-types para obtener las expresiones variables y constantes
                           ;* luego, extender el ambiente de tipos con las variables y constantes
                           (let*
                               ((exps (get-ids-and-types expressions (empty-tenv)))
                                (ids (car exps))
                                (types (cadr exps))
                                (ids-proc (caddr exps))
                                (args-types-proc (cadddr exps))
                                (results-types (car (cddddr exps))))
                             (begin
                               (extend-tenv-rec ids-proc args-types-proc results-types (extend-tenv ids (expand-type-expressions types) (empty-tenv)))
                               )
                             )))))


;* type-of-block-program: <bloque-program> -> type
;* función que chequea el tipo de un bloque de programa
(define type-of-block-program
  (lambda (f-block-program tenv)
    (cases block-program f-block-program
      (fusion-block-program (expression)
                            (type-of-expression expression tenv)))))




;* type-of-expression: <expresión> <type-environment> -> type
;* función que chequea el tipo de una expresión teniendo en cuenta un ambiente de tipos dado
(define type-of-expression
  (lambda (exp tenv)
    (cases expression exp
      (lit-int-exp (_number) int-type)
      (lit-float-exp (_number) float-type)
      (lit-string-exp (_text) string-type)
      (lit-bool-true-exp () bool-type)
      (lit-bool-false-exp () bool-type)
      (fusion-identifier-exp (id) (apply-tenv tenv id))
      (if-exp (test-exp true-exp false-exp)
              (let
                  ((test-type (type-of-expression test-exp tenv))
                   (true-type (type-of-expression true-exp tenv))
                   (false-type (type-of-expression false-exp tenv)))
                (check-equal-type! test-type bool-type test-exp)
                (check-equal-type! true-type false-type true-exp)
                true-type))
      (switch-exp (test-exp cases bodies default-exp)
                  (let ((test-type (type-of-expression test-exp tenv)))
                    (check-equal-type! test-type bool-type test-exp)
                    (let ((case-types (map (lambda (case) (type-of-expression case tenv)) cases)))
                      (let ((body-types (map (lambda (body) (type-of-expression body tenv)) bodies)))
                        (let ((default-type (type-of-expression default-exp tenv)))
                          (let loop ((case-types case-types) (body-types body-types))
                            (if (null? case-types)
                                default-type
                                (begin
                                  (check-equal-type! (car case-types) test-type (car cases))
                                  (check-equal-type! (car body-types) default-type (car bodies))
                                  (loop (cdr case-types) (cdr body-types))))))))))

      (print-exp (_exps)
                 ;? Chequear los tipos de las expresiones
                 (let ((_types (map (lambda (exp) (type-of-expression exp tenv)) _exps)))
                   void-type))

      (lit-proc-exp (types-ids ids body)
                    (type-of-proc-exp types-ids ids body tenv))
      (fusion-app-exp (rator rands)
                      (type-of-application
                       (type-of-expression rator tenv)
                       (types-of-expressions rands tenv)
                       rator rands exp))

      (binary-exp (op rand1 rand2)
                  (let
                      ((rand-types (types-of-expressions (list rand1 rand2) tenv)))
                    (type-of-application
                     (type-of-binary-prim op rand-types)
                     rand-types
                     op (list rand1 rand2) exp)))

      (unary-exp (rator orand)
                 (type-of-application
                  (type-of-unary-prim rator (type-of-expression orand tenv))
                  (list (type-of-expression orand tenv))
                  rator (list orand) exp))


      (locals-exp (vars-exps exp-body bodies) (type-of-block-locals-exp vars-exps exp-body bodies tenv))

      (block-exp (expression expressions) (type-of-block-exp expression expressions tenv))

      (fusion-var-exp (type-exp _id assigned-exp)
                      (let ((assigned-type (type-of-expression assigned-exp tenv)))
                        (check-equal-type! type-exp assigned-type exp)
                        assigned-type))

      (fusion-const-exp (type-exp _id assigned-exp)
                        (let ((assigned-type (type-of-expression assigned-exp tenv)))
                          (check-equal-type! type-exp assigned-type exp)
                          type-exp))

      (fusion-assign-exp (id assigned-exp)
                         (let*
                             ((assigned-type (type-of-expression assigned-exp tenv))
                              (id-type (apply-tenv tenv id)))
                           (check-equal-type! id-type assigned-type exp)
                           bool-type))

      ;? While expression: verificar que la cond-exp sea un booleano y que el body-exp sea void
      (while-exp (cond-exp _body-exp)
                 (let ((cond-type (type-of-expression cond-exp tenv)))
                   (check-equal-type! cond-type bool-type cond-exp)
                   void-type))

      (for-exp (init-exp cond-exp sep-exp _body-exp)
               (let ((_init-type (type-of-expression init-exp tenv))
                     (cond-type (type-of-expression cond-exp tenv))
                     (_sep-type (type-of-expression sep-exp tenv)))
                 (check-equal-type! cond-type bool-type cond-exp)
                 void-type))

      (lit-dict-exp (keys-exp values-exp)
                    (let ((key-types (map (lambda (exp) (type-of-expression exp tenv)) keys-exp))
                          (value-types (map (lambda (exp) (type-of-expression exp tenv)) values-exp)))
                      (if (null? key-types)
                          (dict-type void-type void-type)
                          (if (equal? (length key-types) 1)
                              (dict-type (car key-types) (car value-types))
                              (let ((first-key-type (car key-types))
                                    (first-value-type (car value-types)))
                                (let loop ((key-types key-types) (value-types value-types))
                                  (if (null? key-types)
                                      (dict-type first-key-type first-value-type)
                                      (begin
                                        (check-equal-type! first-key-type (car key-types) exp)
                                        (check-equal-type! first-value-type (car value-types) exp)
                                        (loop (cdr key-types) (cdr value-types))))))))))


      ;! REFACTORIZAR ESTO! EN UNA SOLA FUNCION PARA QUE LO USE VECTORS, LIST Y DICT
      (lit-vector-exp (elems-exp)
                      (let ((elem-types (map (lambda (exp) (type-of-expression exp tenv)) elems-exp)))
                        (if (null? elem-types)
                            (vector-type void-type)
                            (if (equal? (length elem-types) 1)
                                (vector-type (car elem-types))
                                (let ((first-type (car elem-types)))
                                  (let loop ((types elem-types))
                                    (if (null? types)
                                        (vector-type first-type)
                                        (begin
                                          (check-equal-type! first-type (car types) exp)
                                          (loop (cdr types))))))))))
      ;! REFACTORIZAR ESTO!
      (lit-list-exp (elems-exp)
                    (let ((elem-types (map (lambda (exp) (type-of-expression exp tenv)) elems-exp)))
                      (if (null? elem-types)
                          (list-type void-type)
                          (if (equal? (length elem-types) 1)
                              (list-type (car elem-types))
                              (let ((first-type (car elem-types)))
                                (let loop ((types elem-types))
                                  (if (null? types)
                                      (list-type first-type)
                                      (begin
                                        (check-equal-type! first-type (car types) exp)
                                        (loop (cdr types))))))))))




      (vertices-exp (_vertices) vertices-type)
      (edges-exp (_edges) edges-type)
      (graph-exp (_vertices _edges)
                 (let ((vertices-type (type-of-expression _vertices tenv))
                       (edges-type (type-of-expression _edges tenv)))
                   (check-equal-type! (type-of-expression _vertices tenv) vertices-type _vertices)
                   (check-equal-type! (type-of-expression _edges tenv) edges-type _edges)
                   (graph-type vertices-type edges-type)))

      (else 'a)
      )))

(define expand-type-expression
  (lambda (texp)
    ;? Verificar si es una proc-type, si es así, retornar el tipo proc


    (cases type-exp texp
      (type-int-exp () int-type)
      (type-float-exp () float-type)
      (type-string-exp () string-type)
      (type-bool-exp () bool-type)
      (type-proc-exp (_typ) proc-tp)
      (type-vertices-exp () vertices-type)
      (type-edges-exp () edges-type)


      (type-list-exp (texp) (list-type (expand-type-expression texp)))
      (type-vector-exp (texp) (vector-type (expand-type-expression texp)))
      (type-dict-exp (key-exp value-exp) (dict-type (expand-type-expression key-exp) (expand-type-expression value-exp)))
      (type-graph-exp () (graph-type vertices-type edges-type))
      )))


;* expand-type-expressions: Funcion que expande el tipo de las expressiones
;* Si esta es de tipo type, se deja igual

(define expand-type-expressions
  (lambda (texps)
    (map (lambda (texp)
           (if (type? texp)
               texp
               (expand-type-expression texp))) texps)))


;check-equal-type!: <type> <type> <expression> ->
; verifica si dos tipos son iguales, muestra un mensaje de error en caso de que no lo sean
(define check-equal-type!
  (lambda (t1 t2 exp)
    (if (not (equal? t1 t2))
        (eopl:error 'check-equal-type!
                    "Types didn’t match: ~s != ~s in~%~s"
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
      (list-type (elem-type) (list 'list elem-type))
      (vector-type (elem-type) (list 'vector elem-type))
      (dict-type (key-type value-type) (list 'dict key-type value-type))
      (graph-type (vertices-type edges-type) (list 'graph vertices-type edges-type))
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
                                  "~%expected ~s~%got ~s")
                                 exp
                                 (map type-to-external-form arg-types)
                                 (map type-to-external-form rand-types))))
      (else
       (eopl:error 'type-of-expression
                   "Rator not a proc type:~%~s~%had rator type ~s"
                   rator (type-to-external-form rator-type))))))

(define type-of-proc-exp
  (lambda (texps ids body tenv)
    (let ((arg-types (expand-type-expressions texps)))
      (let ((result-type
             (type-of-expression body
                                 (extend-tenv ids arg-types tenv))))
        (proc-type arg-types result-type)))))

;types-of-expressions: (list-of <type-exp>) <tenv> -> (list-of <type>)
; función que mapea la función type-of-expresion a una lista
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (exp) (type-of-expression exp tenv)) rands)))


(define type-of-unary-prim
  (lambda (prim value)
    (cases unary-prim prim
      (unary-neg-exp () (proc-type (list bool-type) bool-type))
      (unary-add-exp ()
                     (cond
                       [(equal? value int-type) (proc-type (list int-type) int-type)]
                       [else (proc-type (list float-type) float-type)]))

      (unary-sub-exp ()
                     (cond
                       [(equal? value int-type) (proc-type (list int-type) int-type)]
                       [else (proc-type (list float-type) float-type)]))
      (string-length-exp () (proc-type (list string-type) int-type))
      (dict-bool-exp () (proc-type (list dict-type) bool-type))
      (dict-make-exp () (proc-type (list dict-type) dict-type))
      (dict-keys-exp ()
                     (cases type (car value)
                       (dict-type (_key-type _value-type) (proc-type (list (dict-type _key-type _value-type)) (list-type _key-type)))
                       (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (dict-values-exp ()
                       (cases type (car value)
                         (dict-type (_key-type _value-type)  (proc-type (list (dict-type _key-type _value-type)) (list-type _value-type)))
                         (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (vector-bool-exp ()
                       (cases type (car value)
                         (vector-type (_elem-type) (proc-type (list (vector-type _elem-type)) bool-type))
                         (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (vector-delete-exp (_pos)
                         (cases type (car value)
                           (vector-type (_elem-type)
                                        (proc-type (list (vector-type _elem-type)) (vector-type _elem-type)))
                           (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (vector-length-exp ()
                         (cases type (car value)
                           (vector-type (_elem-type) (proc-type (list (vector-type _elem-type)) int-type))
                           (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (list-empty-bool-exp ()
                           (cases type (car value)
                             (list-type (_elem-type) (proc-type (list (list-type _elem-type)) bool-type))
                             (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))
      (list-head-exp ()
                     (cases type (car value)
                       (list-type (_elem-type) (proc-type (list (list-type _elem-type)) _elem-type))
                       (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (list-tail-exp ()
                     (cases type (car value)
                       (list-type (_elem-type) (proc-type (list (list-type _elem-type)) (list-type _elem-type)))
                       (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (list-length-exp ()
                       (cases type (car value)
                         (list-type (_elem-type) (proc-type (list (list-type _elem-type)) int-type))
                         (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (list-bool-exp ()
                     (cases type (car value)
                       (list-type (_elem-type) (proc-type (list (list-type _elem-type)) bool-type))
                       (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))
      
      (graph-edges-list-exp ()
                           (cases type (car value)
                             (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type)) (list-type edge-type)))
                             (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (graph-vertices-list-exp ()
                              (cases type (car value)
                                (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type)) (list-type (cadr value))))
                                (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (graph-vertices-exp ()
                          (cases type (car value)
                            (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type)) _vertices-type))
                            (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (graph-edges-exp ()
                       (cases type (car value)
                         (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type)) _edges-type))
                         (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (graph-add-edge-exp (_pair)
                          (cases type (car value)
                            (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type) (list-type (list-type (car value) (cadr value)))) (graph-type _vertices-type _edges-type)))
                            (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))
      
      (graph-add-vertex-exp (_vertex)
                            (cases type (car value)
                              (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type) (car value)) (graph-type _vertices-type _edges-type)))
                              (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (graph-outgoing-neighbors-exp (_vertex)
                                    (cases type (car value)
                                      (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type) (car value)) (list-type (cadr value))))
                                      (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))

      (graph-incoming-neighbors-exp (_vertex)
                                    (cases type (car value)
                                      (graph-type (_vertices-type _edges-type) (proc-type (list (graph-type _vertices-type _edges-type) (car value)) (list-type (cadr value))))
                                      (else (eopl:error 'type-of-unary-prim "Invalid unary prim: ~s" prim))))


      )))

(define type-of-binary-prim
  (lambda (prim values)
    (cases binary-prim prim
      (binary-add-exp ()
                      (cond
                        [(and (equal? (car values) int-type) (equal? (cadr values) int-type))
                         (proc-type (list int-type int-type) int-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type float-type) float-type)]
                        [(and (equal? (car values) string-type) (equal? (cadr values) string-type))
                         (proc-type (list string-type string-type) string-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) int-type))
                         (proc-type (list float-type int-type) float-type)]
                        [(and (equal? (car values) int-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type int-type) float-type)]))
      (binary-sub-exp ()
                      (cond
                        [(and (equal? (car values) int-type) (equal? (cadr values) int-type))
                         (proc-type (list int-type int-type) int-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type float-type) float-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) int-type))
                         (proc-type (list float-type int-type) float-type)]
                        [(and (equal? (car values) int-type) (equal? (cadr values) float-type))
                         (proc-type (list int-type float-type) float-type)]))

      (binary-mul-exp ()
                      (cond
                        [(and (equal? (car values) int-type) (equal? (cadr values) int-type))
                         (proc-type (list int-type int-type) int-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type float-type) float-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) int-type))
                         (proc-type (list float-type int-type) float-type)]
                        [(and (equal? (car values) int-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type int-type) float-type)]))
      (binary-mod-exp ()
                      (cond
                        [(and (equal? (car values) int-type) (equal? (cadr values) int-type))
                         (proc-type (list int-type int-type) int-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type float-type) float-type)]
                        [(and (equal? (car values) float-type) (equal? (cadr values) int-type))
                         (proc-type (list float-type int-type) float-type)]
                        [(and (equal? (car values) int-type) (equal? (cadr values) float-type))
                         (proc-type (list float-type int-type) float-type)]))
      (binary-div-exp () (proc-type (list (car values) (cadr values))
                                    (cond
                                      [(and (equal? (car values) int-type) (equal? (cadr values) int-type)) int-type]
                                      [(and (equal? (car values) float-type) (equal? (cadr values) float-type)) float-type]
                                      [(and (equal? (car values) float-type) (equal? (cadr values) int-type)) float-type]
                                      [(and (equal? (car values) int-type) (equal? (cadr values) float-type)) float-type])
                                    ))
      (binary-eq-exp ()  (proc-type  (list (car values) (cadr values)) bool-type))
      (binary-neq-exp () (proc-type  (list (car values) (cadr values)) bool-type))
      (binary-lt-exp () (proc-type  (list (car values) (cadr values)) bool-type))
      (binary-lte-exp () (proc-type  (list (car values) (cadr values)) bool-type))
      (binary-gt-exp () (proc-type  (list (car values) (cadr values)) bool-type))
      (binary-gte-exp () (proc-type  (list (car values) (cadr values)) bool-type))

      (vector-set-exp (_pos) (check-equal-type! (type-of-expression _pos (empty-tenv)) int-type prim)
                      (proc-type (list (car values) (cadr values)) vector-type))
      (vector-ref-exp ()
                      (cases type (car values)
                        (vector-type (_elem-type) (proc-type (list (car values) int-type) _elem-type))
                        (else (eopl:error 'type-of-binary-prim "Invalid binary prim: ~s" prim))))

      (vector-append-exp ()
                         (cases type (car values)
                           (vector-type (_elem-type) (check-equal-type! _elem-type (cadr values) prim)
                                        (proc-type (list (vector-type _elem-type) (vector-type _elem-type)) (vector-type _elem-type)))
                           (else (eopl:error 'type-of-binary-prim "Invalid binary prim: ~s" prim))))

      (vector-make-exp () (proc-type (list (car values) (cadr values)) vector-type))


      (dict-ref-exp ()
                    ;? Chequear que el tipo de la clave sea igual al tipo de la clave del diccionario
                    (cases type (car values)
                      (dict-type (key-type _value-type)
                                 (check-equal-type! key-type (cadr values)  prim)
                                 (proc-type (list (dict-type key-type _value-type)) _value-type))
                      (else (eopl:error 'type-of-binary-prim "Invalid binary prim: ~s" prim))))

      (dict-set-exp ()
                    (cases type (car values)
                      (dict-type (key-type value-type)
                                 (check-equal-type! (dict-type key-type value-type) (cadr values)  prim)
                                 (proc-type (list (dict-type key-type value-type) (dict-type key-type value-type)) (dict-type key-type value-type)))
                      (else (eopl:error 'type-of-binary-prim "Invalid binary prim: ~s" prim))))




      (list-cons-exp ()
                     (proc-type (list (car values) (cadr values)) (list-type (cadr values))))

      (list-append-exp ()
                       (check-equal-type! (car values) (cadr values) prim)
                       (proc-type (list (list-type (car values)) (list-type (car values))) (list-type (car values))))


      (else 'bin)
      )))

;! Este código es muy repetitivo, se puede refactorizar
(define type-of-block-locals-exp
  (lambda (vars-exps exp-body bodies tenv)
    (let*
        ((rslt (get-ids-and-types vars-exps tenv))
         (ids (car rslt))
         (types (cadr rslt))
         (ids-proc (caddr rslt))
         (args-types-proc (cadddr rslt))
         (results-types (car (cddddr rslt)))
         (new-tenv (extend-tenv-rec ids-proc args-types-proc results-types (extend-tenv ids (expand-type-expressions types) tenv))))
      (type-of-block-exp exp-body bodies new-tenv))))

;type-of-block-exp
;? Body es una lista de expresiones
(define type-of-block-exp
  (lambda (expr expressions tenv)
    (let loop ( (exp (type-of-expression expr tenv)) (exps expressions) )
      (if (null? exps)
          exp
          (let ((exp (car exps)))
            (cases expression exp
              (fusion-identifier-exp (id) (apply-tenv tenv id))
              (else (loop (type-of-expression exp tenv) (cdr exps)))))))))

; get-ids-and-types: <expresión>* -> (list-of (list-of id) (lis-of type))
; función que obtiene los identificadores y tipos de la lista de expresiones en listas separadas
;! Solo acepta fusion-var-exp y fusion-const-exp
;! Refactoriar para no escribir el código dos veces
(define get-ids-and-types
  (lambda (exps tenv)
    (let loop ((exps exps) (ids '()) (types '()) (ids-proc '()) (args-types-proc '()) (results-types '()) )
      (if (null? exps)
          (list ids types ids-proc args-types-proc results-types)
          (let ((exp (car exps)))
            (cases expression exp
              (fusion-var-exp (_type-exp id _assigned-exp)
                              (cases expression _assigned-exp
                                (lit-proc-exp (_types-ids _ids _body)
                                              (let
                                                  ((args-result (get-proc-args-result-types _assigned-exp tenv)))
                                                (cases type-exp _type-exp
                                                  (type-proc-exp (typ)
                                                                 (loop (cdr exps) ids types (cons id ids-proc) (cons (car args-result) args-types-proc) (cons (expand-type-expression typ) results-types)))
                                                  (else
                                                   (eopl:error 'get-ids-and-types "Not a proc expression: ~s" _assigned-exp)))))
                                (else
                                 (begin
                                   (check-equal-type! (expand-type-expression _type-exp) (type-of-expression _assigned-exp tenv) exp)
                                   (loop (cdr exps) (cons id ids) (cons _type-exp types) ids-proc args-types-proc results-types)))))


              (fusion-const-exp (_type-exp id _assigned-exp)
                                (cases expression _assigned-exp
                                  (lit-proc-exp (_types-ids _ids _body)
                                                (let
                                                    ((args-result (get-proc-args-result-types _assigned-exp tenv)))
                                                  (cases type-exp _type-exp
                                                    (type-proc-exp (typ)
                                                                   (loop (cdr exps) ids types (cons id ids-proc) (cons (car args-result) args-types-proc) (cons (expand-type-expression typ) results-types)))
                                                    (else
                                                     (eopl:error 'get-ids-and-types "Not a proc expression: ~s" _assigned-exp)))))
                                  (else
                                   (begin
                                     (check-equal-type! (expand-type-expression _type-exp) (type-of-expression _assigned-exp tenv) exp)
                                     (loop (cdr exps) (cons id ids) (cons _type-exp types) ids-proc args-types-proc results-types)))))
              (else (loop (cdr exps) ids types ids-proc args-types-proc results-types))))))))

; get-proc-args-result-types: recibe una expresión de tipo proc y retorna una lista con los tipos de los argumentos y el tipo de retorno
(define get-proc-args-result-types
  (lambda (proc-exp _tenv)
    (cases expression proc-exp
      (lit-proc-exp (types-ids _ids _body)
                    (let ((arg-types (expand-type-expressions types-ids)))
                      (list arg-types))) ;? Falta retornar el tipo de retorno (Pero no se puede)
      (else (eopl:error 'get-proc-args-result-types "Not a proc expression: ~s" proc-exp)))))



(type-interpreter)
; (interpreter)