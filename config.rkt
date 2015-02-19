#lang racket

(provide get-config)

(define (get-config key)
  (case key
    [('default-type-system) 'pstlc]
    ))
