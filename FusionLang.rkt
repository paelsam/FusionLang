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
    (f-string ("\"" (or letter digit "_" "$" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%") 
                    (arbno (or letter digit "_" "$" "%" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%")) "\"") string)
    (f-string ("'" (or letter digit "_" "$" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%") 
                   (arbno (or letter digit "_" "$" "%" "&" " " "¿" "?" "!" "." "," ";" ":" "-" "+" "*" "/" "\\" "|" "(" ")" "[" "]" "{" "}" "<" ">" "=" "^" "%")) "'") string)
    ))

; Especificación sintáctica (gramática)
(define grammar-fusionlang-interpreter
  '(
    (program (block-global block-program)                             fusion-program)
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



    (expression (f-string)                                            lit-string-exp)
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
    (expression ( assign-op identifier expression )                   fusion-assign-exp)
    (assign-op ("->")                                                 assign-op-exp)

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
    (expression ("BLOCK" "{" (arbno expression) "}")                  block-sequence-exp)
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
                        (lambda (program) "💩")
                        (sllgen:make-stream-parser
                         scanner-spec-fusionlang-interpreter
                         grammar-fusionlang-interpreter)))

;***********************************************************************************

(interpreter)