#lang racket

(require "compiler.rkt")


(run
 (system pstlc)
 (type (λ (a b :: *) a))
 (reduce-step ((λ (a b :: *) b) a))
 )
(newline)
