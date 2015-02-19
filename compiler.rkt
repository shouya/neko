#lang racket

(require racket/match)
(require "config.rkt")



;; (forall @a : @a -> (@a -> @a) -> @a)
(define (compile-pstlc-type type)
  (match type
    [(? symbol?)  (cons 'base-type type)]
    [(list t1 -> t2 ...)
     (cons '->
           (cons (compile-pstlc-type t1)
                 (compile-pstlc-type t2)))]
    [(list t) (compile-pstlc-type t)]))

(define (func-type? t)
  (and (pair? t)
       (eq? (car t) '->)))

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

(define (get-type-system program)
  (define type-system-decl (assq 'type-system program))
  (if type-system-decl
      (cadr type-system-decl)
      (get-config 'default-type-system)))


#|
(define (neko-env-update-type-binding env f)
  (neko-env ()))
|#


(define (compile-neko program)
  (struct neko-env
    (type-bindings
     type-aliases
     const-types)
    )

  (define type-system (get-type-system program))
  (define compile-type
    (match type-system
      ['pstlc compile-pstlc-type]
      ))

  (define (add-const-type env type)
    (match env
      [(neko-env tb ta ct) (neko-env tb ta (cons type ct))]))
  (define (add-type-binding env type-binding)
    (define new-bnd
      (match type-binding
        [(list name ':: type ...) (cons name (compile-type type))]))
    (match env
      [(neko-env tb ta ct)
       (neko-env (cons new-bnd tb) ta ct)]))


  (define (do-line line env)
    (match line
      [(list 'system _ ...)  env]        ; ignored
      [(list 'ctype ts ...)  (foldl add-const-type env ts)]
      [(list 'Gamma tbs ...) (foldl add-type-binding env tbs)]
      )
    )

  (let do-lines ([lines program]
                 [env (neko-env '()     ; type-bindings
                                '()     ; type-aliases
                                '()     ; const-types
                                )])
    (if (empty? lines) env
        (do-lines (cdr lines)
                  (do-line (car lines) env))
        ))
  1
  )

(display (show-type (compile-pstlc-type '(A -> B))))
(newline)
