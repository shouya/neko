#lang racket

(require "compiler.rkt")
(require "utils.rkt")


(run "A sample piece of pstlc system code"
     (system pstlc)
     (annotate a (* -> * -> *))
     (type (λ (a b :: *) a))               ; should return * -> * -> *
     (reduce-step ((λ (a b :: * -> * -> *) b) a))
     (define a (λ (x :: *) x))
     (reduce-step (a b))
     (reduce-full (a b c))
     (type a)
     (undef a)
     (type a)                              ; should back to * -> * -> *
     )

(newline)

(run "A sample piece of xstlc system code"
     (system xstlc)
     (type true)
     )
