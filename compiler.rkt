#lang racket

(require racket/match)
(require "config.rkt")


;;; Type ::= *
;;;        | Type -> Type
;;;        | ( Type )

(define (compile-pstlc-type type)
  (match type
    [(? symbol?)  (cons 'base-type type)]
    [(list t1 -> t2 ...)
     (cons '->
           (cons (compile-pstlc-type t1)
                 (compile-pstlc-type t2)))]
    [(list t) (compile-pstlc-type t)]))


;;; Term ::= TermVar
;;;        | Term Term
;;;        | λ TermVar : Type . Term
;;;        | ( Term )

(define (compile-pstlc-expr expr)
  (define (make-lambda var expr type)
    (list 'λ var type expr))
  (define (make-appl func arg)
    (list 'appl func arg))

  (match expr
    [(? symbol?) (cons 'var expr)]
    [(list 'λ vars ... ':: type ': exprs ...)
     (foldr (curryr make-lambda (compile-pstlc-type type))
            (compile-pstlc-expr exprs)
            vars)]
    [(list func exprs ...+)
     (foldl make-appl
            func
            (map compile-pstlc-expr exprs))]
    [(list expr)
     (compile-pstlc-expr expr)]
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
          (symbol->string expr)
          (paren-quote (show-expr expr))))
    (define str-vars (map symbol->string vars))

    (string-append "λ "
                   (string-join str-vars " ")
                   " : "
                   (show-type type)
                   " . "
                   lambda-expr-str
                   ))
  (define (show-appl func args)
    (define appl-func-str
      (if (lambda? func)
          (paren-quote (show-expr func))
          (show-expr func)))
    (define arg-strs
      (map (λ (a) (if (term-var? a)
                      (show-expr a)
                      (paren-quote (show-expr a))))
           args))
    (string-join (cons appl-func-str arg-strs) " "))

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

(define (get-type-system program)
  (define type-system-decl (assq 'type-system program))
  (if type-system-decl
      (cadr type-system-decl)
      (get-config 'default-type-system)))


#|
(define (neko-env-update-type-binding env f)
  (neko-env ()))
|#
(struct neko-env
  (type-bindings
   type-aliases
   const-types))


(define (deduce-pstlc-type env expr)
  (+ 1 1)
  )


(define (run-neko program)
  (define type-system (get-type-system program))

  (define compile-type
    (match type-system
      ['pstlc compile-pstlc-type]
      ))
  (define compile-expr
    (match type-system
      ['pstlc compile-pstlc-expr]
      ))
  (define deduce-type
    (match type-system
      ['pstlc deduce-pstlc-type]))

  (define (add-const-type env type)
    (match env
      [(neko-env tb ta ct)
       (neko-env tb ta (cons type ct))]))

  (define (add-type-binding env type-binding)
    (define new-bnd
      (match type-binding
        [(list name ':: type ...)
         (cons name (compile-type type))]))
    (match env
      [(neko-env tb ta ct)
       (neko-env (cons new-bnd tb) ta ct)]))

  (define (add-type-alias env new-name orig-name)
    (define alias (cons new-name orig-name))
    (match env
      [(neko-env tb ta ct)
       (neko-env tb
                 (cons alias ta)
                 ct)]))

  (define (query-type env expr)
    (define compiled-expr (compile-expr expr))
    (define deduced-type  (deduce-type env compiled-expr))
    (printf "~a :: ~a"
            (show-expr compiled-expr)
            (show-type deduced-type))
    env)

  (define (do-command line env)
    (match line
      [(list 'system _ ...)  env]        ; ignored
      [(list 'ctype ts ...)  (foldr add-const-type env ts)]
      [(list 'type t orig-t) (add-type-alias env t orig-t)]
      [(list 'Gamma tbs ...) (foldr add-type-binding env tbs)]
      [(list ':type expr)    (query-type env expr)]
      )
    )

  (let do-commands ([lines program]
                    [env (neko-env '()     ; type-bindings
                                   '()     ; type-aliases
                                   '()     ; const-types
                                   )])
    (match lines
      ['() env]
      [(cons line rest-lines)
       (let ([new-env (do-command line env)])
         (do-commands rest-lines new-env))]))
  )

(display (show-type (compile-pstlc-type '(A -> B))))
(newline)
