#lang racket

(require racket/match)
(require "utils.rkt")
(require "common.rkt")
(require "env.rkt")

(require "pstlc.rkt")

(provide compile-type
         compile-term
         normal-form?
         reduce-step
         reduce-full
         deduce-type
         do-command
         init-env)



;;; Type ::= *                ; unit type
;;;        | Type -> Type
;;;        | nat
;;;        | bool
;;;        | ( Type )

(define (compile-type-xstlc type)
  (match type
    ['*              (make-unit-type)]
    ['nat            (make-const-type 'nat)]
    ['bool           (make-const-type 'bool)]
    [(list t1 '-> t2 ...)
     (make-func-type (compile-type t1)
                     (compile-type t2))]
    [(list t) (compile-type t)]
    ))
(set! compile-type compile-type-xstlc)


;;; Term ::= TermVar
;;;        | λ TermVar : Type . Term
;;;        | Term Term
;;;        | ( Term )

(define (compile-term-xstlc term)
  (define (my-make-lambda var expr type)
    (make-lambda var type expr))

  (match term
    [(list 'λ (list vars ... ':: type ...) terms ...)
     (foldr (curryr my-make-lambda (compile-type type))
            (compile-term terms)
            vars)]
    [(? nature-number?) (make-cvalue 'nat term)]
    [(or 'true 'false)  (make-cvalue 'bool term)]
    [(? symbol?)        (make-var term)]
    [(list func terms ...)
     (fold-left make-appl
                (compile-term func)
                (map compile-term terms))]
    [(list term)
     (compile-term term)]
    ))
(set! compile-term compile-term-xstlc)


(define (term-case-xstlc term
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
(set! term-case term-case-xstlc)

(define (reduce-beta-xstlc appl env)
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
(set! reduce-beta reduce-beta-xstlc)

(define (deduce-type-xstlc term env [bnd (empty-type-bnd)])
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

(set! deduce-type deduce-type-xstlc)
