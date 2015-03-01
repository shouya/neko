#lang racket

(require racket/match)
(require (for-syntax racket/syntax
                     syntax/parse))

(provide (all-defined-out))

(struct basic-env (bindings annotations))

(define-syntax-rule (update-env e field-id fn)
  (match e
    [(struct* basic-env     ([field-id v]))
     (struct-copy basic-env e [field-id (fn v)])]
    [_ (error "Error 418: I'm a Teapot!")]))


(define-syntax (define-env-lookup-stack stx)
  (syntax-parse stx
    [(_ prop:id)
     (define/with-syntax add-id    (format-id stx "add-~a"    #'prop))
     (define/with-syntax remove-id (format-id stx "remove-~a" #'prop))
     (define/with-syntax find-id   (format-id stx "find-~a"   #'prop))
     (define/with-syntax field-id  (format-id stx "~as"       #'prop))
     (define/with-syntax field-accessor
       (format-id stx "basic-env-~as"                         #'prop))
     (define/with-syntax id-defined?
       (format-id stx "~a-defined?"                           #'prop))

     #'(begin
         (define (add-id env var val)
           (let* ([rec    (cons var val)]
                  [update (curry cons rec)])
             (update-env env field-id update)))
         (define (remove-id env var)
           (let* ([mtch (λ (name rec) (equal? name (car rec)))]
                  [remv (λ (xs) (remove var xs mtch))])
             (update-env env field-id remv)))
         (define (assoc-id env name)
           (define records (field-accessor env))
           (let recur ([e records])
             (cond
              [(null? e)              #f]
              [(equal? (caar e) name) (car e)]
              [#t                     (recur (cdr e))])))
         (define (find-id env name)
           (let ([pair (assoc-id env name)])
             (if pair (cdr pair) #f)))
         (define id-defined? assoc-id))
     ]))

(define-env-lookup-stack binding)
(define-env-lookup-stack annotation)
