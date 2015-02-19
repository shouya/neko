#lang racket

(provide fold-left
         flip)

(define (fold-left f i xs)
  (if (null? xs) i
      (fold-left f (f i (car xs)) (cdr xs))))

(define (flip f)
  (λ (x y) (f y x)))
