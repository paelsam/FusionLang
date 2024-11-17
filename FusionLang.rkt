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
     ("GLOBALS" "{" (arbno expression ";") "}")                           fusion-block-global)
    (block-program
     ("PROGRAM" "{" "proc" "@main" "=" "function" "(" ")"
                "{" "return" expression "}" "}")                      fusion-block-program)

    (expression (identifier)                                          fusion-identifier-exp)
    (expression
     (type-exp identifier "=" expression)                         fusion-var-exp)
    (expression
     ("const" type-exp identifier "=" expression)                 fusion-const-exp)
    (expression
     ("=>" expression "(" (separated-list expression ",") ")")        fusion-app-exp)



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

    (expression ( unary-prim expression )                             unary-exp)
    (unary-prim ("!")                                                 unary-neg-exp)

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

    ; Primitivas internas de las listas

    (unary-prim ("@empty?")                                           list-empty-bool-exp)
    (expression ("@empty")                                            list-empty-exp)
    (unary-prim ("@head")                                             list-head-exp)
    (unary-prim ("@tail")                                             list-tail-exp)
    (unary-prim ("@make-list")                                        list-cons-exp)
    (unary-prim ("@list?")                                            list-bool-exp)
    (binary-prim ("@append")                                          list-append-exp)

    ; Primitivas internas de los vectores

    (unary-prim ("@vector?")                                          vector-bool-exp)
    (unary-prim ("@make-vector")                                      vector-make-exp)
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
    (expression ("BLOCK" "{" expression (arbno ";" expression ";") "}")       block-exp)
    (expression
     ("LOCALS" "{" (arbno expression ";") "}"
               "{" (arbno expression ";") "}")                            locals-exp)

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
      ("print" "(" (separated-list expression ",") ")")           print-exp)


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
                                  (var-ids (car (car ids-and-exps)))
                                  (var-exps (cadr (car ids-and-exps)))
                                  (const-ids (car (cadr ids-and-exps)))
                                  (const-exps (cadr (cadr ids-and-exps))))
                             (extend-env var-ids var-exps const-ids (list->vector const-exps) env))))))

; eval-block-program: Evalúa un bloque de programa
; Evalua todas la expresiones de un bloque de programa para luego
; evaluar la última expresión que será la función principal (main)

(define eval-block-program
  (lambda (f-block-program env)
    (cases block-program f-block-program
      (fusion-block-program (expression) (eval-expression expression env)))))

; eval-expression: Evalúa una expresión
; Evalua una expresión de FusionLang y retorna el valor resultante
; parámetros: f-expression (expresión de FusionLang); env (ambiente de evaluación)

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (fusion-identifier-exp (id) id)
      (fusion-var-exp (type-exp id rhs-exp) (extend-env (list id) (eval-expression rhs-exp env) '() (make-vector 0) env))
      (fusion-const-exp (type-exp id rhs-exp) (extend-env '() '() (list id) (list->vector (list (eval-expression rhs-exp env))) env))
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
                            (eopl:error "Attempt to apply non-procedure ~s" proc))))
      (lit-string-exp (str) (decode-string str))
      (lit-int-exp (int) int)
      (lit-float-exp (float) float)
      (lit-bool-true-exp () 'True)
      (lit-bool-false-exp () 'False)
      (lit-proc-exp (types ids body) (closure ids body env))
      (lit-list-exp (expressions) (eval-operators expressions env))
      (lit-vector-exp (expressions) (list->vector (eval-operators expressions env)))
      (lit-dict-exp (keys values) (create-dictionary keys values)) ;! Problema con las expresiones
      (list-empty-exp () empty)

      (if-exp (test-exp true-exp false-exp)
              (if (convert-bool-value (eval-expression test-exp env))
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
               (let loop ((i (eval-expression init-exp env)))
                 (if (eval-expression cond-exp env)
                     (begin
                       (for-each (lambda (exp) (eval-expression exp env)) body-exp)
                       (loop (eval-expression update-exp env)))
                     '⨐ ;! Quitar luego
                     )))

      (while-exp (cond-exp body-exp)
                 (let loop ()
                   (if (eval-expression cond-exp env)
                       (begin
                         (for-each (lambda (exp) (eval-expression exp env)) body-exp)
                         (loop))
                       '⨐ ;! Quitar luego
                       )))

      (locals-exp (var-exps body)
                  ;? Evaluar las variables locales
                  (let*
                      ((ids-and-exps (get-ids-and-exps var-exps env))
                       (var-ids (car (car ids-and-exps)))
                       (var-exps (cadr (car ids-and-exps)))
                       (const-ids (car (cadr ids-and-exps)))
                       (const-exps (cadr (cadr ids-and-exps)))
                       (new-env (extend-env var-ids var-exps const-ids (list->vector const-exps) env)))

                    ;? Evaluar las expresiones del cuerpo
                    (for-each (lambda (exp) (eval-expression exp new-env)) body)
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
                 (eval-unary-prim unary-op (eval-expression exp env)))


      (print-exp (expressions)
                 ;? En caso de que en la lista de expresiones haya un identificador
                 ;? Aplicar el apply-env para obtener el valor de la variable
                 (for-each (lambda (exp)
                             (cases expression exp
                               (fusion-identifier-exp (id) (display (deref (apply-env-ref env id))))
                               (else (display (eval-expression exp env))))) expressions)
                 (newline))

      (else (eopl:error "Expresión no válida"))
      )))

(define eval-binary-prim
  (lambda (prim values)
    (cases binary-prim prim
      (binary-add-exp () (cond
                           ((and (string? (car values)) (string? (cadr values))) (string-append (car values) (cadr values)))
                           ((and (number? (car values)) (number? (cadr values))) (+ (car values) (cadr values)))
                           (else (eopl:error "Error to process ~s" values))))
      (binary-sub-exp () (- (car values) (cadr values)))
      (binary-mul-exp () (* (car values) (cadr values)))
      (binary-div-exp () (/ (car values) (cadr values)))
      (binary-eq-exp () (equal? (car values) (cadr values)))
      (binary-neq-exp () (not (equal? (car values) (cadr values))))
      (binary-lt-exp () (< (car values) (cadr values)))
      (binary-lte-exp () (<= (car values) (cadr values)))
      (binary-gt-exp () (> (car values) (cadr values)))
      (binary-gte-exp () (>= (car values) (cadr values)))

      (vector-set-exp (pos) (vector-set (car values) pos (cadr values)))
      (vector-ref-exp () (vector-ref (car values) (cadr values)))
      (vector-append-exp () (cons (vector->list (car values)) (cadr values))) ;! ARREGLAR ESTO

      (dict-ref-exp () (ref-dict (car values) (cadr values)))


      (else 'b))
    ))

(define eval-unary-prim
  (lambda (prim value)
    (cases unary-prim prim
      (unary-neg-exp () (convert-bool-value (not (convert-bool-value value))))
      (string-length-exp () (string-length value))
      (dict-bool-exp () (eval-dict? value))
      (dict-make-exp () (if (is-a-dict? value) value (eopl:error "This is not a dict ~s" value )))
      (dict-keys-exp () (get-keys value))
      (dict-values-exp () (get-values value))
      (vector-make-exp () (if (vector? value) value (eopl:error "This is not a vector ~s" value )))
      (vector-bool-exp () (convert-bool-value (vector? value)))
      (vector-delete-exp (pos) (vector-delete value pos))
      (list-empty-bool-exp () (convert-bool-value (null? value)))
      (list-head-exp () (car value))
      (list-tail-exp () (cdr value))
      (list-cons-exp () (if (list? value) value (eopl:error "This is not a list ~s" value )))
      (else (eopl:error "Error to process ~s" value)))
    ))


;*********************************************************************************
; Diccionarios

(define-datatype entry entry?
  (make-entry (key expression?)
              (value expression?)))

(define-datatype dictionary dictionary?
  (make-dict (dict-entries (list-of entry?))))

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
      (make-entry (key value)
                  key ;? Debería evaluar las expresiones?
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
      (make-entry (key value)
                  value ;? Debería evaluar las expresiones?
                  ))))

(define filter-entries
  (lambda (key entries)
    (if (null? entries) '()
        (let ((entry (car entries)))
          (if (eqv? key (get-key entry))
              (cons entry (filter-entries key (cdr entries)))
              (filter-entries key (cdr entries)))))))

(define ref-dict
  (lambda (dict key)
    (cases dictionary dict
      (make-dict (entries)
                 (let ((entry (filter-entries key entries)))
                   (if (null? entry)
                       (get-value entry)
                       (eopl:error "Key ~s not found" key)))))))


;*********************************************************************************
; Vectores

; vector-delete
; elimina el elemento de la posición pos y desplaza una posición
; a la izquierda todos los elementos a la derecha
; de la posición pos.
; La posición inicial es la posición 0

;! No está eliminando los elementos correctamente
(define vector-delete
  (lambda (vec pos)
    (let ((len (vector-length vec)))
      (let ((new-vec (make-vector (- len 1))))
        (let loop ((i 0))
          (if (>= i len) new-vec
              (begin
                (if (< i pos)
                    (vector-set! new-vec i (vector-ref vec i))
                    (vector-set! new-vec (- i 1) (vector-ref vec i)))
                (loop (+ i 1)))))))))

; vector-set
; Cambia el valor de la posición pos del vector vec por el valor val.

(define vector-set
  (lambda (vec pos val)
    (vector-set! vec pos val)
    vec))

;*********************************************************************************
; Creación de store (almacén de variables)

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
;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

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

;* get-ids-and-exp: Obtiene una lista con los identificadores y valores de las variables y constantes
;* ((ids-vars, vals-vars) (ids-const, vals-consts))
(define get-ids-and-exps
  (lambda (expressions env)
    (let loop ((exps expressions) (var-ids '()) (var-exps '()) (const-ids '()) (const-exps '()))
      (if (null? exps)
          (list (list var-ids var-exps) (list const-ids const-exps))
          (let ((exp (car exps)))
            (cases expression exp
              (fusion-var-exp (type-exp id assigned-exp)
                              (loop (cdr exps) (cons id var-ids) (cons (eval-expression assigned-exp env) var-exps) const-ids const-exps))
              (fusion-const-exp (type-exp id assigned-exp)
                                (loop (cdr exps) var-ids var-exps (cons id const-ids) (cons (eval-expression assigned-exp env) const-exps)))
              (else (loop (cdr exps) var-ids var-exps const-ids const-exps))))))))

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

;! Solucionar: NO ESTÁN IMPLEMENTADAS LAS FUNCIONES RECURSIVAS
(define extend-env-recursively
  (lambda (proc-names identifiers bodies old-env)
    (let*
        ((len (length proc-names))
         (vec (make-vector len))
         (env (extended-env-record proc-names vec '() (make-vector 0) old-env)))
      (for-each
       (lambda (pos ids body)
         (vector-set! vec pos (closure ids body env)))
       (iota len) identifiers bodies)
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

(define true-value?
  (lambda (x) (not (zero? x))))

(define scheme-value? (lambda (v) #t))

;! Dividir esta función en dos
(define convert-bool-value
  (lambda (v)
    (cond
      [(equal? v #t) 'True]
      [(equal? v #f) 'False]
      [(equal? v 'True) #t]
      [(equal? v 'False) #f]
      [else (eopl:error "¿What value is this?")])))



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