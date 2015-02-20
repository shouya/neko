#lang racket

(require racket/match)
(require "utils.rkt")
(require "config.rkt")

;;; Type ::= *
;;;        | Type -> Type
;;;        | ( Type )

(define (compile-xstlc-type type)
  (match type
    [(? symbol?)  (cons 'base-type type)]
    [(list t1 -> t2 ...)
     (cons '->
           (cons (compile-xstlc-type t1)
                 (compile-xstlc-type t2)))]
    [(list t) (compile-xstlc-type t)]))


;;; Term ::= TermVar
;;;        | Term Term
;;;        | λ TermVar : Type . Term
;;;        | ( Term )

(define (compile-xstlc-expr expr)
  (define (make-lambda var expr type)
    (list 'λ var type expr))
  (define (make-appl func arg)
    (list 'appl func arg))

  (match expr
    [(? symbol?) (cons 'var expr)]
    [(list 'λ vars ... ':: type ': exprs ...)
     (foldr (curryr make-lambda (compile-xstlc-type type))
            (compile-xstlc-expr exprs)
            vars)]
    [(list func exprs ...)
     (fold-left make-appl
                (compile-xstlc-expr func)
                (map compile-xstlc-expr exprs))]
    [(list expr)
     (compile-xstlc-expr expr)]
    ))



(define (func-type? t)
  (and (pair? t)
       (eq? (car t) '->)))
(define (term-var? expr)
  (and (pair? expr)
       (eq? (car expr) 'var)))
(define (lambda? expr)
  (and (pair? expr)
       (eq? (car expr) 'λ)))
(define (application? expr)
  (and (pair? expr)
       (eq? (car expr) 'appl)))

(define (var-name expr)  (and (term-var? expr) (cdr expr)))
(define (lambda-var  expr) (and (lambda? expr) (cadr expr)))
(define (lambda-type expr) (and (lambda? expr) (caddr expr)))
(define (lambda-expr expr) (and (lambda? expr) (cadddr expr)))
(define (appl-func expr) (and (application? expr) (cadr expr)))
(define (appl-cant expr) (and (application? expr) (caddr expr)))


(define (show-type type)
  (define (show-func-type t1 t2)
    (let* ([st1  (show-type t1)]
           [st2  (show-type t2)]
           [sst1 (if (func-type? t1)
                     (string-append "(" st1 ")")
                     st1)])
      (string-append sst1 " -> " st2)))

  (match type
    [(cons 'base-type t)        (symbol->string t)]
    [(cons '-> (cons t1 t2))    (show-func-type t1 t2)]
    ))

(define (show-expr expr)
  (define (paren-quote str)
    (string-append "(" str ")"))

  (define (show-lambda vars type expr)
    (define lambda-expr-str
      (if (term-var? expr)
          (symbol->string (var-name expr))
          (paren-quote (show-expr expr))))
    (define str-vars (map symbol->string vars))

    (string-append "λ "
                   (string-join str-vars " ")
                   " : "
                   (show-type type)
                   " . "
                   lambda-expr-str
                   ))
  (define (show-appl func arg)
    (define appl-func-str
      (if (lambda? func)
          (paren-quote (show-expr func))
          (show-expr func)))
    (define arg-str
      (if (term-var? arg)
          (show-expr arg)
          (paren-quote (show-expr arg))))
    (string-join (list appl-func-str arg-str) " "))

  (define (acc-lambda-vars var type expr)
    (if (and (lambda? expr)
             (equal? (lambda-type expr) type))
        (let ([args (acc-lambda-vars (lambda-var  expr)
                                     (lambda-type expr)
                                     (lambda-expr expr))])
          (list (cons var (car args))
                (cadr args)
                (caddr args)))
        (list (list var) type expr)))

  (match expr
    [(list 'λ var type expr)
     (apply show-lambda (acc-lambda-vars var type expr))]
    [(list 'appl t1 t2) (show-appl t1 t2)]
    [(cons 'var v)      (symbol->string v)]
    ))


(struct neko-env
  (type-bindings
   type-aliases
   const-types))

(define (deduce-xstlc-type env expr)
  (+ 1 1)
  )

(define (do-xstlc-command cmd)
  )
