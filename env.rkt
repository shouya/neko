#lang racket

(require racket/match)


(struct basic-env (bindings annotations))

(define-syntax-rule (update-env e field-id fn)
  (match e
    [(struct neko-env      ([field-id v]))
     (struct-copy neko-env e [field-id (fn v)])]
    [(struct basic-env     ([field-id v]))
     (struct-copy basic-env e [field-id (fn v)])]
    [_ (error "Error 418: I'm a Teapot!")]))

(define-syntax-rule (add-binding env var term)
  (update-env env bindings
              (λ (xs) (cons (cons var term) xs))))
(define-syntax-rule (remove-binding env var)
  (update-env env bindings
              (λ (xs) (remove var xs
                              (λ (a b) (equal? a (car b)))))))
(define-syntax-rule (add-annotation env var type)
  (update-env env annotations
              (λ (xs) (cons (cons var type) xs))))
(define-syntax-rule (remove-annotation env var)
  (update-env env annotations
              (λ (xs) (remove var xs
                              (λ (a b) (equal? a (car b)))))))

(define (find-annotation env name)
  (let recur ([e env])
    (cond
     [(null? e)              #f]
     [(equal? (caar e) name) (cdar e)]
     [#t                     (recur (cdr e))])))
(define (find-binding env name)
  (let recur ([e env])
    (cond
     [(null? e)              #f]
     [(equal? (caar e) name) (cdar e)]
     [#t                     (recur (cdr e))])))