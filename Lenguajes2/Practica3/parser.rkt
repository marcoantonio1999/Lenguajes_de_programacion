#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [ (list? sexp)
       (case (first sexp)
         [ (+) (op + (map parse (cdr sexp)) )]
         [ (-) (op - (map parse (cdr sexp)) )]
         [ (*) (op * (map parse (cdr sexp)) )]
         [ (/) (op / (map parse (cdr sexp)) )]
         [ (modulo) (op modulo (list (parse(second sexp))(parse(third sexp)))) ]
         [ (expt) (op expt (list (parse(second sexp))(parse(third sexp)))) ]
         [ (add1) (op add1 (list (parse(second sexp)))) ]
         [ (sub1) (op sub1 (list (parse(second sexp)))) ]
         [ (with) (if(list? (car(cadr sexp)))    (with (bind (cadr sexp)) (parse (caddr sexp)))
                   (error "sintaxis incorrecta")  )]
         [ (with*) (if(list? (car(cadr sexp)))    (with (bind (cadr sexp)) (parse (caddr sexp)))
                   (error "sintaxis incorrecta")  ) ]
         [else error "operacion no esta definida en WAE"]
       )
    ]  
  )
 )
(define (bind l)
  (cond
    [(empty? l) '()]
    [else (append (list (binding (first (car l))(parse (second(car l))) ))(bind (cdr l)) )]
    )
 )