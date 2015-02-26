# neko
An adorable typed calculus langauges for Pure Simply Typed Lambda
Caculus (PSTLC), Extended Pure Simply Typed Lambda Calculus (XSTLC),
System F, and System F-ω!

![neko](http://i.imgur.com/n4j7vuIl.png)

neko was, again, a cute cat-girl.

## syntax

Neko uses s-expression as its syntax. Neko source code can be written
in a quoted racket list.

### types

```
Type ::= *
       | Type -> Type
       | ( Type )
```

**Note:** the unit type `(*)` is rigid. That means it's not for
polymorphism. For example: `(a :: * -> *)` does not accept argument
with type `* -> *`. Polymorphism is supported in System F and System F-ω
via type variables.

### terms

```
Type    ::= <see above definition>
TermVar ::= <any racket symbol>

Term ::= TermVar
       | λ (TermVar ... :: Type) Term     -- lambda abstraction
       | Term Term ...                    -- functional application
       | ( Term )
```

**Note:** lambda terms supports multiple variable lambda as a syntatic
sugar, which means, `λ (v1 v2 ... vn :: T) t` will be expanded to
`λ (v1 :: T) (λ (v2 :: T) ... (λ (vn :: T) t))`.

Functional application curries in neko. `t1 t2 ... tn` is treated as a
syntatic sugar to `((t1 t2) ... ) tn`.


## essential functions

These functions are available for all systems, but notice they may
behave differently.

```racket
;; compiles neko syntax code
(compile-type type ...)
(compile-term term)

;; convert compiled entities to readable string
(show-type type)
(show-expr term)

;; reduction functions
(reduce-step term env)
(reduce-full term env)

;; provide an pre-initialized environment for reduction
(init-env)

;; handle commands specified for different systems, this function does
;; actions and returns new `env`.
(do-command command env)
```


## references
* [An Introduction to System F - PPS](http://www.pps.univ-paris-diderot.fr/~miquel/slides/got03-1.pdf)
* [A Short Introduction to Systems F and F-ω](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.137.2063&rep=rep1&type=pdf)


# license
MIT, see `LICENSE` for details.
