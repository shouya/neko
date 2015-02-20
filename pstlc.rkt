#lang racket

(require racket/match)

(provide do-command
         )

(define (do-command line env)
  (match line
    [(list 'system _ ...)  env]        ; ignored
    [(list 'ctype ts ...)  (foldr add-const-type env ts)]
    [(list 'type t orig-t) (add-type-alias env t orig-t)]
    [(list 'Gamma tbs ...) (foldr add-type-binding env tbs)]
    [(list ':type expr)    (query-type env expr)]
    ))


;;; Type ::= *
;;;        | Type -> Type
;;;        | ( Type )



;;; Term ::= TermVar
;;;        | Term Term
;;;        | λ TermVar : Type . Term
;;;        | ( Term )
