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

In Hy:

```lisp
(import [adderall.dsl [*]])
(def q (fresh "q"))

(run q (eitherᵍ (=ᵒ q :tea)
                (bothᵍ (=ᵒ q :coffee)
                       succeed)))
;; => [:tea :coffee]

(run q (eitherᵍ (=ᵒ q :tea)
                (bothᵍ (=ᵒ q :coffee)
                       fail)))
;; => [:tea]
```

Or in Python:
```python
import hy
from adderall.dsl import *

q = fresh ("q")

run (q, eitherg (eq (q, "tea"),
                 bothg (eq (q, "coffee"),
                        succeed)))
## => ['tea', 'coffee']

run (q, eitherg (eq (q, "tea"),
                 bothg (eq (q, "coffee"),
                        fail)))
## => ['tea']
```

License
-------

All the code is licensed under the GNU General Public License (v3+).
