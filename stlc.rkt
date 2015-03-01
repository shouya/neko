#lang racket

(require racket/match)
(require "utils.rkt")
(require "common.rkt")
(require "env.rkt")

(provide compile-type
         compile-term
         normal-form?
         reduce-step
         reduce-full
         deduce-type
         do-command
         init-env)


(define (init-env)
  (basic-env
   '()                                  ; annos
   '()))                                ; defs


;;; Type ::= *                ; unit type
;;;        | Type -> Type
;;;        | ( Type )

(define (compile-type type)
  (match type
    ['*              (make-unit-type)]
    ['int            (make-const-type 'int)]
    ['bool           (make-const-type 'bool)]
    [(list t1 '-> t2 ...)
     (make-func-type (compile-type t1)
                     (compile-type t2))]
    [(list t) (compile-type t)]
    ))


;;; Term ::= TermVar
;;;        | λ TermVar : Type . Term
;;;        | Term Term
;;;        | ( Term )

(define (compile-term term)
  (define (my-make-lambda var expr type)
    (make-lambda var type expr))

  (match term
    [(list 'λ (list vars ... ':: type ...) terms ...)
     (foldr (curryr my-make-lambda (compile-type type))
            (compile-term terms)
            vars)]
    [(? nature-number?) (make-cvalue 'int term)]
    [(or 'true 'false)  (make-cvalue 'bool term)]
    [(? symbol?)        (make-var term)]
    [(list func terms ...)
     (fold-left make-appl
                (compile-term func)
                (map compile-term terms))]
    [(list term)
     (compile-term term)]
    ))


(define (normal-form? term env)
  (match term
    [(? (value? term env)) #t]
    [(list 'appl t1 t2)
     (if (lambda? t1) #f (normal-form? t2))]
    ))

(define (value? term env)
  (match term
    [(? lambda?)   #t]
    [(? term-var?) (if (binding-defined? env (var-name term))
                       (value? (find-binding env term) env)
                       #f)]
    [_             #f]))

(define (term-case term
                   #:var  [var  #f]
                   #:lamb [lamb #f]
                   #:appl [appl #f]
                   #:cval [cval #f])
  (define (try-appl func . args)
    (if (false? func)
        (error (format "case for term (~a) not included"
                       (show-expr term)))
        (apply func args)))
  (match term
    [(? term-var?)    (try-appl var (var-name term))]
    [(? lambda?)      (try-appl lamb
                                (lambda-var  term)
                                (lambda-type term)
                                (lambda-expr term))]
    [(? application?) (try-appl appl
                                (appl-func term)
                                (appl-cant term))]
    [(? cvalue?)      (try-appl cval
                                (cvalue-type term)
                                (cvalue-value term))]))

(define (reduce-beta appl env)
  (assert (application? appl))
  (define lamb (appl-func appl))
  (define term (appl-cant appl))
  (assert (lambda? lamb))

  (define (subst term var sub)
    (define (for-var name)
      (if (equal? name var)
          sub
          term))
    (define (for-lamb lvar type expr)
      (if (equal? lvar var)
          term
          (make-lambda lvar type (subst expr var sub))))
    (define (for-appl func cant)
      (make-appl (subst func var sub)
                 (subst cant var sub)))
    (define (for-cval type val) (make-cvalue type val))
    (term-case term
               #:var  for-var
               #:lamb for-lamb
               #:appl for-appl
               #:cval for-cval))
  (let* ([lamb-var   (lambda-var  lamb)]
         [arg-type   (lambda-type lamb)]
         [lamb-body  (lambda-expr lamb)]
         [term-type  (deduce-type term env)])
    (if (type-compatible? term-type arg-type)
        (subst lamb-body lamb-var term)
        (report-type-incompatible term-type arg-type
                                  "type error on beta reduction"))))
(define (reduce-beta-able? term env)
  (if (not (application? term)) #f
      (let ([func (appl-func term)])
        (if (lambda? func) #t   #f))))

(define (reduce-li1 term env)
  (define-values (func cant)
    (values (appl-func term)
            (appl-cant term)))
  (make-appl (reduce-step func env) cant))
(define (reduce-li2 term env)
  (define-values (func cant)
    (values (appl-func term)
            (appl-cant term)))
  (make-appl func (reduce-step cant env)))
(define (reduce-li term env)
  (define-values (func cant)
    (values (appl-func term)
            (appl-cant term)))
  (make-appl (reduce-step func env)
             (reduce-step cant env)))
(define (reduce-li1-able? term env)
  (if (not (application? term)) #f
      (reducible? (appl-func term) env)))
(define (reduce-li2-able? term env)
  (if (not (application? term)) #f
      (reducible? (appl-cant term) env)))

(define (reduce-li-alt func cant env)
  (let* ([reduced (reduce-li1 func cant env)]
         [func2   (appl-func reduced)]
         [cant2   (appl-cant reduced)])
    (reduce-li2 func2 cant2 env)))

;; transivity
(define (reduce-trs term env) (reduce-step (reduce-step term env) env))
(define (reduce-trs-able? term env)
  (if (not (reducible? term env)) #f
      (let ([reduced (reduce-step term env)])
        (reducible? term env))))

;; reflexivity
(define (reduce-ref term env) term)
(define (reduce-ref-able? term env) #t)

(define (reduce-var term env)
  (define name (var-name term))
  (find-binding env name))
(define (reduce-var-able? term env)
  (and (term-var? term)
       (binding-defined? env (var-name term))))

(define (reducible? term env)
  (ormap (λ (f) (f term env))
         (list reduce-beta-able?
               reduce-li1-able?
               reduce-li2-able?
               reduce-var-able?)))


(define (reduce-step term env)
  ;; notice: the order of reduction does not matter
  (define reduction-proc
    (cdr (assf (λ (pred) (if (pred term env) #t #f))
               (list (cons reduce-beta-able? reduce-beta)
                     (cons reduce-li1-able?  reduce-li1)
                     (cons reduce-li2-able?  reduce-li2)
                     (cons reduce-var-able?  reduce-var)
                     (cons (const #t)        '())))))
  (if (null? reduction-proc)
      (error (format "term ~a is not reducible" (show-expr term)))
      (reduction-proc term env)))

(define (reduce-full term env)   ;; probably undetermined
  (if (not (reducible? term env)) term
      (reduce-full (reduce-step term env) env)))


(define (empty-type-bnd) '())
(define (deduce-type term env [bnd (empty-type-bnd)])
  (define (var-case name)
    (define (find-in-bnd)
      (let ([var-bnd (assoc name bnd)])
        (and var-bnd (cdr var-bnd))))
    (define (find-in-env)
      (find-annotation env name))
    (define (auto-assign-unit-type)
      (make-unit-type))
    (define (report-not-found name)
      (let ([var-str (symbol->string name)])
        (error (format "variable ~a not found" var-str))))
    (ormap (λ (p) (apply p '()))
           (list find-in-bnd
                 find-in-env
                 auto-assign-unit-type
                 report-not-found))
    )
  (define (lamb-case var type expr)
    (define new-bnd (cons (cons var type) bnd))
    (make-func-type type
                    (deduce-type expr env new-bnd)))
  (define (appl-case func cant)
    (build-appl-type (deduce-type func env bnd)
                     (deduce-type cant env bnd)))
  (define (cval-case type-name val)
    (make-const-type type-name))

  (term-case term
             #:var  var-case
             #:lamb lamb-case
             #:appl appl-case
             #:cval cval-case))

(define (build-appl-type t1 t2)
  (if (not (func-type? t1))
      (error (format "error: ~a is not a function" t1))
      (let ([dom-type (func-type-dom   t1)]
            [cod-type (func-type-codom t1)])
        (if (not (type-compatible? t2 dom-type))
            (report-type-incompatible t2 dom-type)
            cod-type))))

;; predicate for get <: expect
(define (type-compatible? get expect)
  (equal? get expect))

(define (print-type env term)
  (define compiled-term (compile-term term))
  (define deduced-type  (deduce-type compiled-term env))
  (printf "~a :: ~a"
          (show-expr compiled-term)
          (show-type deduced-type))
  env)


(define (do-command line env) #f)
