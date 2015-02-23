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


(define (get-type-system program)
  (define type-system-decl (assq 'type-system program))
  (if type-system-decl
      (cadr type-system-decl)
      (get-config 'default-type-system)))

(struct neko-env
  (type-bindings
   type-aliases
   const-types))

(define (deduce-xstlc-type env expr)
  (+ 1 1)
  )

(define (do-xstlc-command cmd)
  )
