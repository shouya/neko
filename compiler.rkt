#lang racket

(require racket/match)
(require "config.rkt")
(require "utils.rkt")

(require (prefix-in pstlc: "pstlc.rkt")
         (prefix-in xstlc: "xstlc.rkt"))


#|
(define (neko-env-update-type-binding env f)
  (neko-env ()))
|#



(define (get-type-system program)
  (define type-system-decl (assq 'type-system program))
  (if type-system-decl
      (cadr type-system-decl)
      (get-config 'default-type-system)))

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
    (define new-const-types
      (cons type (neko-env-const-types env)))
    (struct-copy neko-env env
                 [const-types new-const-types]))

  (define (add-type-binding env type-binding)
    (define new-bnd
      (match type-binding
        [(list name ':: type ...)
         (cons name (compile-type type))]))
    (define new-type-bindings
      (cons new-bnd (neko-env-type-bindings env)))
    (struct-copy neko-env env
                 [type-bindings new-type-bindings]))

  (define (add-type-alias env new-name orig-name)
    (define alias (cons new-name orig-name))
    (define new-type-aliases
      (cons alias (neko-env-type-aliases env)))
    (struct-copy neko-env env
                 [type-aliases new-type-aliases]))



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

; (display (show-expr (compile-pstlc-expr '(λ A C :: B : E F))))
(display (show-expr (compile-pstlc-expr
                     '(λ x :: T : (λ y :: T : X) E (F G) H))))
; (show-expr (compile-pstlc-expr '(E F G H)))
(newline)
