#lang racket

(require "compiler.rkt")


(run
 (system pstlc)
 (annotate a (* -> * -> *))
 (type (λ (a b :: *) a))                ; should return * -> * -> *
 (reduce-step ((λ (a b :: * -> * -> *) b) a))
 (define a (λ (x :: *) x))
 (reduce-step (a b))
 (reduce-full (a b c))
 (type a)
 (undef a)
 (type a)
 )
(newline)
