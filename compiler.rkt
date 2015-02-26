#lang racket

(require racket/match)
(require "config.rkt")
(require "utils.rkt")
(require "common.rkt")

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
  (dynamically-bind-system type-system)


  (define (compiler-do-command line env)
    (define-syntax-rule (harmless stmt ...)
      (begin stmt ... env))

    (define (cmd-reduce reduce-proc term)
      (let* ([cterm   (compile-term term)]
             [reduced (reduce-proc cterm env)]
             [str     (show-expr reduced)])
        (harmless (printf "~a\n" str))))

    (define (cmd-normal? term)
      (let* ([cterm (compile-term term)]
             [norm? (normal-form? cterm)]
             [sterm (show-expr cterm)]
             [pred  (string-append (if norm? "is" "isn't")
                                   "in normal form.")])
        (harmless (printf "~a ~a") sterm pred)))

    (define (cmd-annotate var type)
      (let* ([ctype (compiled-type)])
        (add-annotation env var ctype)))
    (define (cmd-unanno var) (remove-annotation env var))

    (define (cmd-deduce-type term)
      (let* ([cterm (compiled-term term)]
             [type  (deduce-type cterm)]
             [sterm (show-expr cterm)]
             [stype (show-type type)])
        (harmless (printf "~a :: ~a" sterm stype))))

    (define (cmd-define var term)
      (let* ([cterm (compile-term term)]
             [ctype (deduce-type cterm)]
             [env1  (add-annotation env var ctype)]
             [env2  (add-binding env1 var cterm)])
        env2))
    (define (cmd-undef var)
      (let* ([env1 (remove-binding env var)]
             [env2 (remove-annotation env1 var)])
        env2))

    ;; * `(system <type-system>)`
    ;; * `(annotate var type)`
    ;; * `(unanno var)`
    ;; * `(reduce-step term)`
    ;; * `(reduce-full term)`
    ;; * `(normal? term)`
    ;; * `(type term)`
    ;; * `(define var term)`
    ;; * `(undef var)`

    (match line
      [(list 'system _)          env]
      [(list 'annotate var type) (cmd-annotate var type)]
      [(list 'unanno var)        (cmd-unanno var)]
      [(list 'reduce-step term)  (cmd-reduce reduce-step term)]
      [(list 'reduce-full term)  (cmd-reduce reduce-full term)]
      [(list 'normal? term)      (cmd-normal? term)]
      [(list 'type term)         (cmd-deduce-type term)]
      [(list 'define var term)   (cmd-define var term)]
      [(list 'undef var)         (cmd-undef var)]
      [_                         #f]
      )) ; end of compiler-do-command


  (define (do-cmd line env)
    (let ([rst1 (do-command line env)])
      (if (not (false? rst1)) rst1
          (let ([rst2 (compiler-do-command line env)])
            (if (not (false? rst2)) rst2
                (error "command (~a) not supported" line))))))

  (let do-commands ([lines program]
                    [env (init-env)])
    (let* ([line    (car lines)]
           [rest    (cdr lines)]
           [new-env (do-cmd line env)])
      (do-commands rest new-env))))

(display (show-type (compile-pstlc-type '(A -> B))))
(newline)

; (display (show-expr (compile-pstlc-expr '(λ A C :: B : E F))))
(display (show-expr (compile-pstlc-expr
                     '(λ x :: T : (λ y :: T : X) E (F G) H))))
; (show-expr (compile-pstlc-expr '(E F G H)))
(newline)
