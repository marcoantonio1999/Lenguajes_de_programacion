#lang plai
;; Ejercicio 1
;; Predicado que recibe un número natural n y devuelve verdadero si n es par, falso en otro caso.
;; esPar? : numner -> boolean
(define (esPar? n) ...)

;; Ejercicio 2
;; Función que recibe un número natural n y devuelve una lista en orden ascendente,
;; con los números menores o iguales a n.
;; menores : number -> (listof number)
(define (menores n) ...)

;; Ejercicio 3
;; Función que recibe un número natural n y devuelve una lista en orden ascendente,
;; con los números pares desde 0 hasta n.
;; pares: number -> (listof number)
(define (pares n) ...)

;; Ejercicio 4
;; Función que recibe un número n y calcula la suma de los primeros n números naturales al cuadrado.
;; Esta función utiliza a fórmula conocida para esta cuenta.
;; suma-cuadrados: number -> number
(define (suma-cuadrados n) ...)

;; Ejercicio 5
;; Función recursiva, que calcula la suma de los primeros n números naturales al cuadrado.
;; Esta función no utiliza la fórmula conocida, ni directa ni indirectamente.
;; suma-cuadrados: number -> number
(define (suma-cuadradosR n) ...)

;; Ejercicio 6
;; Función que recibe los términos a, b y c,  de una expresión cuadrática y decide si la expresión tiene
;; raíces reales. La función verifica que el discriminante sea mayor o igual a cero.
;; raicesReales? : number number number -> boolean
(define (raicesReales? a b c) ...)

;; Ejercicio 7
;; Función que recibe tres números a, b y c y devuelve la primer raíz de la fórmula general (sumando la raíz cuadrada)
;; general1: number number number -> number
(define (general1 a b c) ...)

;; Ejercicio 8
;; Función que recibe tres números a, b y c y devuelve la segunda raíz de la fórmula general (restando la raíz cuadrada)
;; general2: number number number -> number
(define (general2 a b c) ...)

;; Ejercicio 9
;; Función que nos da una lista invertida de la lista pasada como parámetro
;; reversa-lista: (listof a) -> (listof a)
(define (reversa-lista cadena)  ...)

;; Ejercicio 10
;; Predicado que nos dice si una lista contiene elementos que forman un palíndromo
;; palindromo-lista?: (listof a) -> Boolean
(define (palindromo? lista) ...)