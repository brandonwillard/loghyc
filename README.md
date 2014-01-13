Adderall
========

[![Build Status](https://travis-ci.org/algernon/adderall.png?branch=master)](https://travis-ci.org/algernon/adderall)

This library contains a (work in progress) implementation of
[miniKanren][mk] in [Hy][hylang]. It is ugly, dumb, slow and a lot of
other things, but sooner or later, it will get better. I hope. Or
maybe it won't.

 [mk]: http://minikanren.org/
 [hylang]: http://hylang.org/

Example
-------

```lisp
(import [adderall.dsl [*]])
(require adderall.dsl)

(run* [q] (eitherᵍ (≡ q :tea)
                   (bothᵍ (≡ q :coffee)
                          succeed)))
;; => [:tea :coffee]

(run* [q] (eitherᵍ (≡ q :tea)
                   (bothᵍ (≡ q :coffee)
                          fail)))
;; => [:tea]

(defreader h [_] #ss)
(defn lovesᵒ [u v] (≡ v #hy))
(run* [q] (lovesᵒ :algernon #hy) (≡ q true))
;; => [True]
```

More examples can be found in the [test suite][t:generic], and in
particular, the tests [adapted][t:trs] from
[The Reasoned Schemer][trs].

 [t:generic]: https://github.com/algernon/adderall/blob/master/tests/adderall_test.hy
 [t:trs]: https://github.com/algernon/adderall/blob/master/tests/reasoned_schemer.hy
 [trs]: http://mitpress.mit.edu/books/reasoned-schemer

License
-------

All the code is licensed under the GNU General Public License (v3+).
