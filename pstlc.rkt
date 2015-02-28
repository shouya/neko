#lang racket

(require racket/match)
(require "utils.rkt")
(require "common.rkt")

(provide compile-type
         compile-term
         normal-form?
         reduce-step
         reduce-full
         deduce-type
         do-command
         init-env)


(void
 ;; sample pstlc program
 ;; (λ y :: * -> * : y) (λ x :: * : x)
 '(
   (system pstlc)
   (annotate x :: int)
   (print-type ((λ y :: * -> * : y)) (λ x :: * : x))
   ))

(struct pstlc-env
  (type-assignments))                   ; unused

(define (init-env)
  (pstlc-env '() ;; type assignments (unused)
             ))

(define-syntax update-env
  (syntax-rules ()
    [(_ env fld accessor f)
     (let* ([old-fld (accessor env)]
            [new-fld (f old-fld)])
       (struct-copy pstlc-env env [fld new-fld]))]))

(define (add-type-assignment var type env)
  (update-env env type-assignments pstlc-env-type-assignments
              ((curry cons) '(var . type))))

;;; Type ::= *
;;;        | Type -> Type
;;;        | ( Type )

(define (compile-type type)
  (match type
    ['*              (make-unit-type)]
    [(list t1 '-> t2 ...)
     (make-func-type (compile-type t1)
                     (compile-type t2))]
    [(list t) (compile-type t)]))



;;; Term ::= TermVar
;;;        | λ TermVar : Type . Term
;;;        | Term Term
;;;        | ( Term )

(define (compile-term term)
  (define (my-make-lambda var expr type)
    (make-lambda var type expr))

  (match term
    [(? symbol?) (make-var term)]
    [(list 'λ (list vars ... ':: type ...) terms ...)
     (foldr (curryr my-make-lambda (compile-type type))
            (compile-term terms)
            vars)]
    [(list func terms ...)
     (fold-left make-appl
                (compile-term func)
                (map compile-term terms))]
    [(list term)
     (compile-term term)]
    ))


(define (normal-form? term)
  (match term
    [(? value?) #t]
    [(list 'appl t1 t2)
     (if (lambda? t1) #f (normal-form? t2))]
    ))

(define (value? term)
  (match term
    [(? lambda?)   #t]
    [(? term-var?) #t]
    [_             #f]))

(define (term-case term var lamb appl)
  (match term
    [(? term-var?)    (var (var-name term))]
    [(? lambda?)      (lamb (lambda-var  term)
                            (lambda-type term)
                            (lambda-expr term))]
    [(? application?) (appl (appl-func term)
                            (appl-cant term))]))

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
    (term-case term for-var for-lamb for-appl))
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

(define (reduce-li1 func cant env)
  (make-appl (reduce-step func env) cant))
(define (reduce-li2 func cant env)
  (make-appl func (reduce-step cant env)))
(define (reduce-li func cant env)
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

(define (reduce-trs term env) (reduce-step (reduce-step term env) env))
(define (reduce-trs-able? term env)
  (if (not (reducible? term env)) #f
      (let ([reduced (reduce-step term env)])
        (reducible? term env))))

(define (reduce-ref term env) term)
(define (reduce-ref-able? term env) #t)

(define (reducible? term env)
  (ormap (λ (f) (f term env))
         (list reduce-beta-able?
               reduce-li1-able?
               reduce-li2-able?)))


(define (reduce-step term env)
  ;; notice: the order of reduction does not matter
  (define reduction-proc
    (cdr (assf (λ (pred) (if (pred term env) #t #f))
               (list (cons reduce-beta-able? reduce-beta)
                     (cons reduce-li1-able?  reduce-li1)
                     (cons reduce-li2-able?  reduce-li2)
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
    (let ([var-bnd (assoc name bnd)]
          [var-str (symbol->string name)])
      (if (false? var-bnd)
          (error (format "variable ~a not bound" var-str))
          (cdr var-bnd))))
  (define (lamb-case var type expr)
    (define new-bnd (cons (cons var type) bnd))
    (make-func-type type
                    (deduce-type expr env new-bnd)))
  (define (appl-case func cant)
    (build-appl-type (deduce-type func env bnd)
                     (deduce-type cant env bnd)))
  (term-case term var-case lamb-case appl-case))

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
