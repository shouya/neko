#lang racket

(require "utils.rkt")

(provide (all-defined-out))

(define (predicate type)
  (λ (x) (and (pair? x) (eq? (car x) type))))
(define (accessor pred? locate)
  (λ (x) (and (pred? x) (locate x))))


;; -- terms --
(define term-var?    (predicate 'var))
(define lambda?      (predicate 'λ))
(define application? (predicate 'appl))

(define var-name    (accessor term-var?    cdr))
(define lambda-var  (accessor lambda?      cadr))
(define lambda-type (accessor lambda?      caddr))
(define lambda-expr (accessor lambda?      cadddr))
(define appl-func   (accessor application? cadr))
(define appl-cant   (accessor application? caddr))

(define (make-var name)             (cons 'var name))
(define (make-lambda var type term) (list 'λ var type term))
(define (make-appl func cant)       (list 'appl func cant))

;; -- types --
(define func-type?   (predicate '->))
(define unit-type?   (predicate '*))

(define func-type-dom   (accessor func-type? cadr))
(define func-type-codom (accessor func-type? cddr))

(define (make-unit-type) (cons '* '()))
(define (make-func-type t1 t2) (cons '-> (cons t1 t2)))



(define (dynamically-bind-system system)
  (define (ns-name type)
    (string->symbol (string-append (symbol->string system)
                                   ":"
                                   (symbol->string type))))
  (define (def-ns type)
    `(define type ,(ns-name type)))
  (display "asdfasdf"
           (list 'begin
              (def-ns 'compile-type)
              (def-ns 'compile-term)
              (def-ns 'normal-form?)
              (def-ns 'reduce-step)
              (def-ns 'reduce-full)
              (def-ns 'do-command)
              (def-ns 'init-env))))



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
    [(cons '* _)                "*"]
    ))



(define (show-expr expr)
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
    [(cons 'var v)      (symbol->string v)])
  )


(define (common-commands? command)
  (define common-commands '(system))
  (member command common-commands))

(define (handle-common-command command env)
  (match command
    [(list 'system _) env]))

(define (report-type-incompatible get expect [msg "incompatible type"])
  (error (format "~a: expect ~a, get ~a" msg
                 (show-type expect)
                 (show-type get))))
