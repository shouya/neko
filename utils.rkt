#lang racket

(provide fold-left          ;; same as foldl in haskell
         flip               ;; seems not used so far, same as haskell
         paren-quote
         undefined
         assert
         )

(define (fold-left f i xs)
  (if (null? xs) i
      (fold-left f (f i (car xs)) (cdr xs))))

(define (flip f)
  (λ (x y) (f y x)))

(define (paren-quote str)
  (string-append "(" str ")"))

(define (undefined . _)
  (error "undefined function"))

(define (assert expr)
  (if expr #t (error "assertion failed!")))
