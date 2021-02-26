#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE-Value
;; (define (lookup name ds)
(define (lookup name ds) (type-case DefrdSub ds
    [mtSub() (error "No se encontro:" name)]
    [aSub(bound-name bound-value rest-ds)
         (if (symbol=? bound-name name)
             (interp bound-value ds)
             (lookup name rest-ds))]) )


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)(cond
    [(list? expr) (interp (car expr) ds)]
    [else (type-case CFWBAE expr
            [id (i) (lookup i ds)]
            [num (n) n]
            [op (o l)
                (cond
                  [(equal? o +) (foldl + 0 (map (lambda (x)(interp x ds)) l))]
                  [(equal? o -) (if (= (length (map (lambda (x)(interp x ds)) l)) 1) (- (interp (car l) ds)) (resta (map (lambda (x) (interp x ds)) l)))]
                  [(equal? o *)(foldl o 1 (map (lambda (x) (interp x ds)) l))]
                  [(equal? o /)(foldl * 1 (append (list (interp (car l) ds)) (map (lambda (x) (expt (interp x ds) -1)) (cdr l))))]
                  [(equal? o modulo) (if (= (length l) 2) (o (interp (car l) ds)(interp (cadr l) ds))(error "modulo es aridad 2 :c"))]
                  [(equal? o expt) (if (= (length l) 2) (o (interp (car l) ds)(interp (cadr l) ds)) (error "expt es aridad 2 :c"))]
                  [(equal? o add1) (if (= (length l) 1) (o (interp (car l) ds)) (error "add1 es aridad 1 :c"))]
                  [(equal? o sub1) (if (= (length l) 1) (o (interp (car l) ds)) (error "sub1 es aridad 1 :c"))])]
            [bool (n) n]
            [iF (test-expr then-expr else-expr)
                 (if (interp test-expr ds)
                     (interp then-expr ds)
                     (interp else-expr ds))]
            [fun (bound-id bound-body)
                 (closure bound-id bound-body ds)]
            [app (fun-expr arg-expr)
                 (local ([define fun-val (interp fun-expr ds)]
                         [define arg-val arg-expr])
                   (type-case CFWBAE-Value fun-val
                     [closure (closure-param closure-body closure-env)
                               (interp closure-body
                                       (aSub (car closure-param) (car arg-val) (creaenv (cdr closure-param) (cdr arg-val) closure-env)) )] ;se crea el ambiente
                     [else (error "Solo se aplica a funciones")]))])]))

(define (creaenv param valor closure)
  (cond
    [(empty? param) closure]
    [else (aSub (car param) (car valor) (creaenv (cdr param) (cdr valor) closure))]))

(define (resta lista )
  (cond
    [(empty? lista) 0]
    [(= (length lista) 1) (car lista)]
    [else (- (car lista) (cadr lista) (resta (cddr lista)))]
    ))
