#lang racket/base
;; sample neko program

(display
  '(
    (system system-f)
    (ctype T1 T2 T3) ;; constant types

    (type Nat
          (forall @a : @a -> (@a -> @a) -> @a))
    (def 0 (L @a :
              l x :: @a :
              l f :: @a -> @ a :
              x))
    (def succ (l n : Nat
                 L @a :
                 l z0 :: @a :
                 l zf :: @a -> @a :
                 zf (n z0 zf)
                 ))

    (:type 0)         ;; => should print: forall @a . @a -> (@a -> @a) -> @a
    (:type (succ 0))  ;; => should print: forall @a . @a -> (@a -> @a) -> @a
    (:type succ)      ;; => should print: (forall @a . @a -> (@a -> @a) -> @a) ->
                      ;;                  (forall @a . @a -> (@a -> @a) -> @a)
    ))

(void
 '(
   (system pstlc) ;; pure simple typed lambda calculi

   (ctype T1 T2 T3)
   (type T4 T3)

   (Gamma [f1 :: T1 -> T2]
          [f2 :: T2 -> T3]
          [c  :: (T2 -> T3) -> (T1 -> T2) -> (T1 -> T3)])

   (:type (c f1 f3)) ; => T1 -> T3

   ))
