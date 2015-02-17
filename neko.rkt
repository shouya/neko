#lang racket/base
;; sample neko program

(display
  '(

    (ctype T1 T2 T3) ;; constant types

    (type Nat
          (forall @a : @a -> (@a -> @a) -> @a))
    (def 0 (L @a :
              l x :: @a :
              l f :: @a -> @ a :
              x))
    (def succ (l n : Nat
                 L @a :
                 l z0 :: @a
                 l zf :: @a -> @a
                 zf (n z0 zf)
                 ))

    (:type 0)  ;; => should print: forall @a . @a -> (@a -> @a) -> @a


    ))

(+ 1 1)
