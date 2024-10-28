#lang eopl

; FusionLang

; Integrantes:
; Elkin Samir Angulo Panameño
; Leonardo Cuadro Lopez

#|
? Denotation Values: int, float, string, bool, proc, list, vector, dict, graph, edges y node. 
? Expresed Values: int, float, string, bool, proc, list, vector, dict, graph, edges y node. 

<program> ::= <expression> program(exp)

<expression> := <int> number-int(num)
             := <float> number-float(num)
             := <identifier> ::= <letter> | {<letter> | 0..9}*  identifier(id)
             := <string> string(str)
             := <bool> ::= True | False bool(b)
             

<letter> ::= A..Z | a..z

|#