# neko
An adorable typed calculus langauges for Simply Typed Lambda
Caculus (STLC), System F, and System F-ω!

![neko](http://i.imgur.com/n4j7vuIl.png)

neko was, again, a cute cat-girl.

## syntax

Neko uses s-expression as its syntax. Neko source code can be written
in a quoted racket list.

### types

```
Type ::= '*
       | Type '-> Type ('-> Type)*
       | '( Type ')
```

**Note:** function type is right associative, `t1 -> t2 -> t3` is
equivalent to `t1 -> (t2 -> t3)`.

The unit type `(*)` is rigid. That means it's not for
polymorphism. For example: `(a :: * -> *)` does not accept argument
with type `* -> *`. Polymorphism is supported in System F and System F-ω
via type variables.



### terms

```
Type    ::= <see above definition>
TermVar ::= <any racket symbol>

Term ::= TermVar
       | 'λ '( TermVar ... ':: Type ') Term     -- lambda abstraction
       | Term Term ...                          -- functional application
       | '( Term ')
```

**Note:** lambda terms supports multiple variable lambda as a syntatic
sugar, which means, `λ (v1 v2 ... vn :: T) t` will be expanded into
`λ (v1 :: T) (λ (v2 :: T) ... (λ (vn :: T) t) ...)`.

Functional application curries in neko. `t1 t2 t3 ... tn` is treated as a
syntatic sugar to `( ... ((t1 t2) t3) ... ) tn`.


### commands

* `(system <type-system>)`: specify the type system. This command must be the first line of a program. Valid type systems is one of `'(stlc system-f system-f-omega)`.
* `(annotate var type)`: annotate an identifier with given type.
* `(unanno var)`: delete an annotated identifier
* `(reduce-step term)`: reduce a term for one step, with current environment.
* `(reduce-full term)`: reduce a term to normal form, it's not garenteeded to terminate for systems with fix-point operator.
* `(normal? term)`: print if a term is in normal form.
* `(type term)`: show the deduced type of a term.
* `(define var term)`: defining an identifier with some value.
* `(undef var)`: delete an defined identifier.

### program structure

neko program contains a list of commands. those commands will be
executed orderly. commands are independent each other and the
environment will carry forward.


## essential functions for a system

These functions are available for all systems, but notice they may
behave differently. You won't need these information unless you want to
look into the implementation or you're going to extend neko with a new
system.

```racket
;; compiles neko syntax code
(compile-type type)
(compile-term term)

;; convert compiled entities to readable strings
;; these two functions are implemented in `common.rkt`.
(show-type type)
(show-expr term)

;; reduction functions
(reduce-step term env)
(reduce-full term env)
(normal-form? term env)

;; types deduction
(deduce-type term env)

;; provide an pre-initialized environment for reduction
(init-env)

;; handle commands specified for different systems, this function does
;; actions and returns new `env`.
(do-command command env)
```


## references
* [An Introduction to System F - PPS](http://www.pps.univ-paris-diderot.fr/~miquel/slides/got03-1.pdf)
* [A Short Introduction to Systems F and F-ω](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.137.2063&rep=rep1&type=pdf)
* (Software Foundations) [STLC: Simply Typed Lambda Calclus](http://www.cis.upenn.edu/~bcpierce/sf/current/Stlc.html)
* [Type systems for programming languages](http://gallium.inria.fr/~remy/mpri/cours1.pdf)


# license
MIT, see `LICENSE` for details.
