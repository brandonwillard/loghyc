;; adderall - miniKanren in Hy
;; Copyright (C) 2014, 2015  Gergely Nagy <algernon@madhouse-project.org>
;;
;; This library is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program. If not, see <http://www.gnu.org/licenses/>.

(import [cons [cons car cdr]])
(import [adderall.dsl [*]]
        [adderall.internal [*]])
(import [tests.schemer.common [*]])

(require [hy.contrib.walk [let]])
(require [adderall.dsl [*]])
(require [tests.schemer.common [*]])


(frame "2.3" [(+ '() [#U 0 #U 1])]
       (run* [r]
             (fresh [v w]
                    (== (let [x v
                              y w]
                             `(~x ~y))
                        r))))

(frame "2.6" ['a]
       (run* [r]
             (firstᵒ ['a 'c 'o 'r 'n None] r)))

(frame "2.7" [True]
       (run* [q]
             (firstᵒ ['a 'c 'o 'r 'n] 'a)
             (≡ True q)))

(frame "2.8" ['pear]
       (run* [r]
             (fresh [x y]
                    (firstᵒ [r y] x)
                    (≡ 'pear x))))

(frame "2.11" [['grape 'a]]
       (run* [r]
             (fresh [x y]
                    (firstᵒ ['grape 'raisin 'pear] x)
                    (firstᵒ [['a] ['b] ['c]] y)
                    (≡ (cons x y) r))))

(frame "2.15" ['c]
       (run* [r]
             (fresh [v]
                    (restᵒ ['a 'c 'o 'r 'n] v)
                    (firstᵒ v r))))

(frame "2.18" [[['raisin 'pear] 'a]]
       (run* [r]
             (fresh [x y]
                    (restᵒ ['grape 'raisin 'pear] x)
                    (firstᵒ [['a] ['b] ['c]] y)
                    (≡ (cons x y) r))))

(frame "2.19" [True]
       (run* [q]
             (restᵒ ['a 'c 'o 'r 'n] ['c 'o 'r 'n])
             (≡ True q)))

(frame "2.20" ['o]
       (run* [x]
             (restᵒ ['c 'o 'r 'n] [x 'r 'n])))

(frame "2.21" [['a 'c 'o 'r 'n]]
       (run* [l]
             (fresh [x]
                    (restᵒ l ['c 'o 'r 'n])
                    (firstᵒ l x)
                    (≡ 'a x))))

(frame "2.22" [[['a 'b 'c] 'd 'e]]
       (run* [l]
             (consᵒ ['a 'b 'c] ['d 'e] l)))

(frame "2.23" ['d]
       (run* [x]
             (consᵒ x ['a 'b 'c] ['d 'a 'b 'c])))

(frame "2.24" [['e 'a 'd 'c]]
       (run* [r]
             (fresh [x y z]
                    (≡ ['e 'a 'd x] r)
                    (consᵒ y ['a z 'c] r))))

(frame "2.25" ['d]
       (run* [x]
             (consᵒ x ['a x 'c] ['d 'a x 'c])))

(frame "2.26" [['d 'a 'd 'c]]
       (run* [l]
             (fresh [x]
                    (≡ ['d 'a x 'c] l)
                    (consᵒ x ['a x 'c] l))))

(frame "2.27" [['d 'a 'd 'c]]
       (run* [l]
             (fresh [x]
                    (consᵒ x ['a x 'c] l)
                    (≡ ['d 'a x 'c] l))))

(frame "2.29" [['b 'e 'a 'n 's]]
       (run* [l]
             (fresh [d x y w s]
                    (consᵒ w ['a 'n 's] s)
                    (restᵒ l s)
                    (firstᵒ l x)
                    (≡ 'b x)
                    (restᵒ l d)
                    (firstᵒ d y)
                    (≡ 'e y))))

(frame "2.32" []
       (run* [q]
             (emptyᵒ ['grape 'raisin 'pear])
             (≡ True q)))

(frame "2.33" [True]
       (run* [q]
             (emptyᵒ [])
             (≡ True q)))

(frame "2.34" [[]]
       (run* [x]
             (emptyᵒ x)))

(frame "2.38" []
       (run* [q]
             (eqᵒ 'pear 'plum)
             (≡ True q)))

(frame "2.39" [True]
       (run* [q]
             (eqᵒ 'plum 'plum)
             (≡ True q)))

(frame "2.52" [[#U 0 #U 1 'salad]]
       (run* [r]
             (fresh [x y]
                    (≡ r [x y 'salad]))))

(frame "2.54" [True]
       (run* [q]
             (pairᵒ
               `(~q ~q)
               #_(cons q q))
             (≡ True q)))

(frame "2.55" []
       (run* [q]
             (pairᵒ [])
             (≡ True q)))

(frame "2.56" []
       (run* [q]
             (pairᵒ 'pair)
             (≡ True q)))

(frame "2.57" [(cons #U 0 #U 1)]
       (run* [x]
             (pairᵒ x)))

(frame "2.58" [#U 0]
       (run* [r]
             (pairᵒ `(~r pear)
               #_(cons r 'pear))))
