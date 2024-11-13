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
    (identifier ("@" (or letter "_" "$" "/" "&" "?") (arbno (or letter digit "_" "$" "%" "&" "?"))) symbol)
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
    (program (expression)                                             fusion-program)
    ; (program (block-global block-program)                             fusion-program)
    (block-global
     ("GLOBALS" "{" (arbno expression) "}")                           fusion-block-global)
    (block-program
     ("PROGRAM" "{" (arbno expression) "}")                           fusion-block-program)

    (expression (identifier)                                          fusion-identifier-exp)
    (expression
     (type-exp identifier "=" expression ";")                         fusion-var-exp)
    (expression
     ("const" type-exp identifier "=" expression ";")                 fusion-const-exp)
    (expression
     ("=>" expression "(" (separated-list expression ",") ")" ";")    fusion-app-exp)



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
    ;! Pregunta: Cómo se invocan a las primitivas de los vectores si los parámetros se pasan por valor?

    (unary-prim ("@vector?")                                          vector-bool-exp)
    (unary-prim ("@make-vector")                                      vector-make-exp)
    (binary-prim ("@ref-vector")                                      vector-ref-exp) ;? Esto recibe 2 argumentos (vector, index)
    (binary-prim ("@set-vector" "[" number-int "]")                   vector-set-exp) ;? Esto recibe 2 argumentos (vector, value)
    (unary-prim ("@append-vector")                                    vector-append-exp)
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
    (expression ("BLOCK" "{" expression (arbno expression) "}")                  block-sequence-exp)
    (expression
     ("LOCALS" "{" (arbno type-exp identifier "=" expression ";") "}"
               "{" (arbno expression) "}")                            locals-exp)

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
      ("print" "(" (separated-list expression ",") ")" ";")           print-exp)


    ))

; eval-program: Evalúa un programa FusionLang
; Cambiar el llamado de los ambientes de evaluación
(define eval-program
  (lambda (f-program env)
    (cases program f-program
      (fusion-program (expression)
                      (eval-expression expression (empty-env))))))
; (define eval-program
;   (lambda (f-program env)
;     (cases program f-program
;       (fusion-program (block-global block-program)
;                       (eval-block-global block-global env)
;                       (eval-block-program block-program env)))))

; eval-block-global: Evalúa un bloque de variables globales
; Evalua todas las expresiones de un bloque global para luego guardarlas en
; ambiente global (ambiente anterior al almbiente vacío)

(define eval-block-global
  (lambda (f-block-global env)
    (cases block-global f-block-global
      (fusion-block-global (expressions)
                           ;! Debe guardar las variables en el ambiente global
                           (eval-expression expressions env)))))

; eval-block-program: Evalúa un bloque de programa
; Evalua todas la expresiones de un bloque de programa para luego
; evaluar la última expresión que será la función principal (main)

(define eval-block-program
  (lambda (f-block-program env)
    (cases block-program f-block-program
      (fusion-block-program (expressions)
                            ;! Debe guardar las variables en el ambiente global
                            (eval-expression expressions env)))))

; eval-expression: Evalúa una expresión
; Evalua una expresión de FusionLang y retorna el valor resultante
; parámetros: f-expression (expresión de FusionLang); env (ambiente de evaluación)

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (fusion-identifier-exp (id) id)
      (fusion-assign-exp (id exp)
                         (begin
                           (set-ref!
                            (apply-env-ref env id)
                            (eval-expression exp env))
                           (lit-bool-true-exp)))
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

      (binary-exp (binary-op exp1 exp2)
          (eval-binary-prim binary-op (eval-operators (list exp1 exp2) env)))
      
      (unary-exp (unary-op exp)
        (eval-unary-prim unary-op (eval-expression exp env)))


      (else (eopl:error "Expresión no válida"))
      )))

(define eval-binary-prim
  (lambda (prim values)
    (cases binary-prim prim
      (binary-add-exp () (
        (cond 
          ((and (string? (car values)) (string? (cadr values))) (string-append (car values) (cadr values)))
          ((and (number? (car values)) (number? (cadr values))) (+ (car values) (cadr values))))
      ))
      (binary-sub-exp () (- (car values) (cadr values)))
      (binary-mul-exp () (* (car values) (cadr values)))
      (binary-div-exp () (/ (car values) (cadr values)))
      (binary-eq-exp () (equal? (car values) (cadr values)))
      (binary-neq-exp () (not (equal? (car values) (cadr values))))
      (binary-lt-exp () (< (car values) (cadr values)))
      (binary-lte-exp () (<= (car values) (cadr values)))
      (binary-gt-exp () (> (car values) (cadr values)))
      (binary-gte-exp () (>= (car values) (cadr values)))


      (else 'b))
  ))

(define eval-unary-prim
  (lambda (prim value)
    (cases unary-prim prim
      (unary-neg-exp () (convert-bool-value (not (convert-bool-value value))))
      (else 'a))
  ))



;*********************************************************************************
; Diccionarios
(define-datatype dictionary dictionary?
  (dict (key expression?)
        (value expression?)))

; create-dictionary: Crea una lista de datatypes de dictionary
(define create-dictionary
  (lambda (keys values)
    (if (and (null? keys) (null? values))  
      '()
      (cons (dict (car keys) (car values)) (create-dictionary (cdr keys) (cdr values))))))


;*********************************************************************************
; Creación de store (almacén de variables)

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

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
           (eval-expression op env))
         ops)))

;*********************************************************************************
;* Construyendo ambiente

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (identifiers (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (ids vals env)
    (extended-env-record ids (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names identifiers bodies old-env)
    (let
        ((len (length proc-names)))
      (let
          ((vec (make-vector len)))
        (let
            ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len) identifiers bodies)
          env)))))

(define apply-env
  (lambda (env id)
    (deref (apply-env-ref env id))))

(define apply-env-ref
  (lambda (env id)
    (cases environment env
      (empty-env-record ()
                        (eopl:error "Variable ~s is not defined" (symbol->string id) ))
      (extended-env-record (identifiers values env)
                           (let ((pos (rib-find-position id identifiers)))
                             (if (number? pos)
                                 (a-ref pos values)
                                 (apply-env-ref env id)))))))


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
               (eval-expression body (extend-env ids args env))))))


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
                        (lambda (program) (eval-program program empty-env))
                        (sllgen:make-stream-parser
                         scanner-spec-fusionlang-interpreter
                         grammar-fusionlang-interpreter)))

;***********************************************************************************

(interpreter)