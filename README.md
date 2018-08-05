Adderall
========

[![Build Status](https://img.shields.io/travis/algernon/adderall/master.svg?style=flat-square)](https://travis-ci.org/algernon/adderall)
[![Downloads](https://img.shields.io/pypi/dm/adderall.svg?style=flat-square)](https://pypi.python.org/pypi/adderall)
[![Version](https://img.shields.io/pypi/v/adderall.svg?style=flat-square)](https://pypi.python.org/pypi/adderall)

This library contains an implementation of [miniKanren][mk] in
[Hy][hylang]. It is a naive implementation, speed isn't the primary
concern.

 [mk]: http://minikanren.org/
 [hylang]: http://hylang.org/

Example
-------

```hy
(import [adderall.dsl [*]])
(require adderall.dsl)

(run* [q] (condᵉ [(≡ q 'tea)]
                 [(≡ q 'coffee) succeed]))
;; => ['tea 'coffee]

(run* [q] (condᵉ [(≡ q 'tea)]
                 [(≡ q 'coffee) fail]))
;; => ['tea]

(import [getpass [getuser]])

(deftag h [_] "#hy")
(defn lovesᵒ [u v] (≡ v #h y))
(run* [who what] (lovesᵒ who what)
                 (≡ who (getuser))
                 (≡ what #h y))
;; => [['algernon' '#hy']]
```

More examples can be found in the [test suite][t:generic], and in
particular, the tests [adapted][t:trs] from
[The Reasoned Schemer][trs].

 [t:generic]: https://github.com/algernon/adderall/blob/master/tests/adderall_test.hy
 [t:trs]: https://github.com/algernon/adderall/blob/master/tests/schemer/
 [trs]: http://mitpress.mit.edu/books/reasoned-schemer

Adderall also comes with a set of extra functions, which are there
mostly to show the power of the system, or for fun:

```hy
(import [adderall.dsl [*]]
        [adderall.extra.zebra [*]])
(require adderall.dsl)
(require adderall.extra.zebra)

(run* [water-drinker horse-owner] (zebraᵖ water-drinker horse-owner))
;; => [['norvegian 'japanese]]
```

For a practical use, see [Hydiomatic][hydiomatic], a static analyser
and code transformer built upon Adderall, or [wynck][wynck], a tool to
reason about your window layout.

 [hydiomatic]: https://github.com/algernon/hydiomatic
 [wynck]: https://github.com/algernon/wynck

License
-------

All the code is licensed under the GNU Lesser General Public License
(v3+).
