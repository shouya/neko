#lang racket

(require racket/match)


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


(display (show-type (compile-pstlc-type '(A -> B))))
(newline)
