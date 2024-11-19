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
    (fusion-string ("\"" (or letter digit "_" "$" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%") 
                         (arbno (or letter digit "_" "$" "%" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%")) "\"") string)
    (fusion-string ("'" (or letter digit "_" "$" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%") 
                        (arbno (or letter digit "_" "$" "%" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%")) "'") string)
    ))

; Especificación sintáctica (gramática)
(define grammar-fusionlang-interpreter
  '(
    (program (block-global block-program)                             fusion-program)
    (block-global
     ("GLOBALS" "{" (arbno expression ";") "}")                       fusion-block-global)
    (block-program
     ("PROGRAM" "{" "proc" "main" "=" "function" "(" ")"
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
    (type-exp ("proc")                                                type-proc-exp)
    (type-exp ("list" "<" type-exp ">")                               type-list-exp)
    (type-exp ("vector" "<" type-exp ">")                             type-vector-exp)
    (type-exp ("dict" "<" type-exp "," type-exp ">")                  type-dict-exp)

    ;? Implementación de tipos para grafos dirigidos
    (type-exp ("graph")                                               type-graph-exp)
    (type-exp ("edges")                                               type-edges-exp)
    (type-exp ("vertices")                                            type-vertex-exp)

    ; Primitivas internas de las listas

    (unary-prim ("@empty?")                                           list-empty-bool-exp)
    (expression ("@empty")                                            list-empty-exp)
    (unary-prim ("@head")                                             list-head-exp)
    (unary-prim ("@tail")                                             list-tail-exp)
    (binary-prim ("@make-list")                                        list-cons-exp)
    (unary-prim ("@list?")                                            list-bool-exp)
    (binary-prim ("@append")                                          list-append-exp)

    ; Primitivas internas de los vectores

    (unary-prim ("@vector?")                                          vector-bool-exp)
    (binary-prim ("@make-vector")                                     vector-make-exp)
    (binary-prim ("@ref-vector")                                      vector-ref-exp) ;? Esto recibe 2 argumentos (vector, index)
    (binary-prim ("@vector-set" "[" number-int "]")                   vector-set-exp) ;? Esto recibe 2 argumentos (vector, value)
    (binary-prim ("@append-vector")                                   vector-append-exp)
    (unary-prim ("@delete-val-vector" "[" number-int "]")             vector-delete-exp)

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
               "{" (arbno expression ";") "}")                        locals-exp)

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

    (unary-prim ("@vertices")                                         graph-vertices-exp)
    (unary-prim ("@edges")                                            graph-edges-exp)
    (unary-prim ("@outgoin-neighbors" expression)                                graph-outgoing-neighbors-exp)
    (unary-prim ("@incoming-neighbors" expression)                               graph-incoming-neighbors-exp)
    ;! Esto es raro XD
    (unary-prim ("@add-edge" expression)                              graph-add-edge-exp)



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
                             (extend-env-recursively
                              proc-names proc-args proc-bodies
                              const-proc-names const-proc-args const-proc-bodies
                              (extend-env var-ids var-exps const-ids (list->vector const-exps) env)))))))

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
      (fusion-var-exp (_type-exp id rhs-exp) (extend-env (list id) (eval-expression rhs-exp env) '() (make-vector 0) env))
      (fusion-const-exp (_type-exp id rhs-exp) (extend-env '() '() (list id) (list->vector (list (eval-expression rhs-exp env))) env))
      (fusion-assign-exp (id exp)
                         (if (is-const? env id)
                             (eopl:error "Variable ~s is a constant" id)
                             (begin
                               (set-ref!
                                (apply-env-ref env id)
                                (eval-expression exp env))
                               (convert-bool-value #t))
                             ))
      (fusion-app-exp (exp args)
                      (let ((proc (deref (apply-env-ref env (eval-expression exp env))))
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
                        (eval-expression (list-ref bodies-exp true-case) env)
                        (eval-expression default-exp env)
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

      (locals-exp (var-exps body)
                  ;? Evaluar las variables locales
                  (let*
                      (
                       (ids-and-exps (get-ids-and-exps var-exps env))
                       (var-ids (car (car ids-and-exps))) (var-exps (cadr (car ids-and-exps)))
                       (const-ids (car (cadr ids-and-exps)))(const-exps (cadr (cadr ids-and-exps)))
                       (proc-names (car (caddr ids-and-exps))) (proc-args (cadr (caddr ids-and-exps)))
                       (proc-bodies (caddr (caddr ids-and-exps))) (const-proc-names (car (cadddr ids-and-exps)))
                       (const-proc-args (cadr (cadddr ids-and-exps))) (const-proc-bodies (caddr (cadddr ids-and-exps)))
                       (new-env (extend-env-recursively
                                 proc-names proc-args proc-bodies
                                 const-proc-names const-proc-args const-proc-bodies
                                 (extend-env var-ids var-exps const-ids (list->vector const-exps) env))))

                    ;? Evaluar las expresiones del cuerpo
                    (for-each (lambda (exp) (eval-identifier exp new-env)) body)
                    #t ;? No sé que retornar
                    )
                  )

      (block-exp (expression expressions)
                 (let loop ((acc (eval-expression expression env)) (exps expressions))
                   (if (null? exps)
                       acc
                       (loop (eval-expression (car exps) env) (cdr exps)))))


      (binary-exp (binary-op exp1 exp2)
                  (let ((args (eval-operators (list exp1 exp2) env)))
                    (eval-binary-prim binary-op args)))

      (unary-exp (unary-op exp)
                 (eval-unary-prim unary-op (eval-identifier exp env) env))


      (print-exp (expressions)
                 ;? En caso de que en la lista de expresiones haya un identificador
                 ;? Aplicar el apply-env para obtener el valor de la variable
                 ;? Si el print tiene varias expresiones, se imprimen todas separadas por un espacio
                 (for-each (lambda (exp)
                             (display (eval-identifier exp env))
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


      (else (eopl:error "Expresión no válida"))
      )))

(define eval-binary-prim
  (lambda (prim values)
    (cases binary-prim prim
      (binary-add-exp () (apply-operator + string-append values))
      (binary-sub-exp () (- (car values) (cadr values)))
      (binary-mul-exp ()
                      (* (car values) (cadr values)))
      (binary-div-exp () (/ (car values) (cadr values)))
      (binary-eq-exp () (apply-operator equal? string=? values))
      (binary-neq-exp () (not (equal? (car values) (cadr values))))
      (binary-lt-exp () (apply-operator < string<? values))
      (binary-lte-exp () (apply-operator <= string<=? values))
      (binary-gt-exp ()  (apply-operator > string>? values))
      (binary-gte-exp () (apply-operator >= string>=? values))

      (vector-set-exp (pos) (vector-set (car values) pos (cadr values)))
      (vector-ref-exp () (vector-ref (car values) (cadr values)))
      (vector-append-exp () (vector-append (car values) (cadr values)))
      (vector-make-exp () (make-vector (car values) (cadr values)))

      (list-cons-exp () (map (lambda (_val) (cadr values)) (iota (car values))))

      (dict-ref-exp () (ref-dict (car values) (cadr values)))
      (dict-set-exp () (dict-set (car values) (cadr values)))

      (dict-append-exp () (append-dict (car values) (cadr values)))

      (else 'b))
    ))

(define eval-unary-prim
  (lambda (prim value env)
    (cases unary-prim prim
      (unary-neg-exp () (convert-bool-value (not (convert-bool-value value))))
      (unary-add-exp () (+ value 1))
      (unary-sub-exp () (- value 1))
      (string-length-exp () (string-length value))
      (dict-bool-exp () (eval-dict? value))
      (dict-make-exp () (if (is-a-dict? value) value (eopl:error "This is not a dict ~s" value )))
      (dict-keys-exp () (get-keys value))
      (dict-values-exp () (get-values value))
      (vector-bool-exp () (convert-bool-value (vector? value)))
      (vector-delete-exp (pos) (vector-delete value pos))
      (list-empty-bool-exp () (convert-bool-value (null? value)))
      (list-head-exp () (car value))
      (list-tail-exp () (cdr value))

      (graph-vertices-exp () (cases expression value
                                               (graph-exp (v _edges) v)
                                               (else (eopl:error "Invalid expression"))))
      (graph-edges-exp () (cases expression value
                            (graph-exp (_vertices edges) edges)
                            (else (eopl:error "Invalid expression"))))

      (graph-add-edge-exp (_pair) 
        (cases expression _pair
          (lit-list-exp (expressions) (add-edge value (map (lambda (exp) (eval-expression exp env)) expressions)))
          (else (eopl:error "Invalid expression"))))
      
      (graph-outgoing-neighbors-exp (vertex) (vecinos-salientes value (eval-expression vertex env)))
      (graph-incoming-neighbors-exp (vertex) (vecinos-entrantes value (eval-expression vertex env)))
        

      (else (eopl:error "Error to process ~s" value)))
    ))


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

(define unparse-dict
  (lambda (dict env)
    (cases dictionary dict
      (make-dict (entries)
                 (let loop ((entries entries))
                   (if (null? entries) "{}"
                       (string-append "{" (unparse-entry (car entries) env) ", " (loop (cdr entries)) "}")))
                 ))))

(define unparse-entry
  (lambda (e env)
    (cases entry e
      (make-entry (key value)
                  (string-append (eval-identifier key env) " : " (eval-identifier value env))
                  ))))

(define append-dict
  (lambda (dict-to-append dict-to-append-with)
    (cases dictionary dict-to-append
      (make-dict (entries)
                 (cases dictionary dict-to-append-with
                   (make-dict (entries-with)
                              (make-dict (append entries entries-with))))))))


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
                                   (cons (eval-expression assigned-exp env) var-exps) const-ids
                                   const-exps proc-names proc-args proc-bodies
                                   const-proc-names const-proc-args const-proc-bodies)))
              (fusion-const-exp (type-exp id assigned-exp)
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
                                     (cons (eval-expression assigned-exp env) const-exps)
                                     proc-names proc-args proc-bodies
                                     const-proc-names const-proc-args const-proc-bodies)))
              (else
               (loop
                (cdr exps) var-ids
                var-exps const-ids
                const-exps proc-names
                proc-args proc-bodies
                const-proc-names const-proc-args const-proc-bodies))))))))

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
      [else (eopl:error "Error to process ~s" values)])))

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
               (eval-expression body (extend-env ids args '() (make-vector 0) env))))))


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

;***********************************************************************************

(interpreter)