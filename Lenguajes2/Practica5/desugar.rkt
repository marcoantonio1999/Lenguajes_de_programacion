#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr) (cond
    [(empty? sexpr) '()]
    [(list? sexpr) (append (list (desugar (car sexpr))) (desugar (cdr sexpr)))]
    [else (type-case SCFWBAE sexpr
            [numS (n) (num n)]
            [boolS (b) (bool b)]
            [iFS (test-expr then-expr else-expr)
                 (iF (desugar test-expr )
                     (desugar then-expr )
                     (desugar else-expr ))]
            [opS (o l) (op o (desugar l))]
            [condS (cases) (if (list? cases)
                                      (to-IF cases)
                                      (error "algo paso"))]
            [withS (bins body) (app (fun (map (lambda (x)  (binding-id x)) bins)
                                                (desugar body)) (map (lambda (x) (desugar (binding-value x))) bins))]
            [withS* (bins body) (local ([define param (map (lambda (x) (binding-id x)) bins)]
                                        [define values (map (lambda (x)
                                                             (if (numS? (binding-value x)) (desugar (binding-value x))
                                                                (let ([var (map (lambda (y) (cond [(equal? (idS (first y)) (binding-value x)) (second y)]))
                                                                      (map (lambda (w) (list (binding-id w) (desugar (binding-value w)))) bins))])
                                                                     (if (void? (car var))  (error "variable libre") (car var))) )) bins)])
                                 (app (fun param (desugar body)) values))]

            [funS (params body) (fun  (desugar params) (desugar body))]
            [appS (fun-sexpr arg-sexpr) (app (desugar fun-sexpr) (desugar arg-sexpr))]
            [idS (i) (id i)])]))

(define (to-IF lista)
  (cond
    [(empty? lista) '()]
    [else (type-case Condition (car lista)
            [condition (test then) (iF (desugar test) (desugar then) (to-IF (cdr lista)))]
            [else-cond (then) (desugar then)])]))