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

(import [adderall.dsl [*]]
        [tests.schemer.common [*]])
(require adderall.dsl)
(require tests.schemer.common)

(frame "2.6" ['a]
       (run* [r]
             (firstᵒ (list* 'a 'c 'o 'r 'n nil) r)))

(frame "2.7" [true]
       (run* [q]
             (firstᵒ ['a 'c 'o 'r 'n] 'a)
             (≡ true q)))

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
                    (restᵒ ['grapre 'raisin 'pear] x)
                    (firstᵒ [['a] ['b] ['c]] y)
                    (≡ (cons x y) r))))

(frame "2.19" [true]
       (run* [q]
             (restᵒ ['a 'c 'o 'r 'n] ['c 'o 'r 'n])
             (≡ true q)))

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
             (≡ true q)))

(frame "2.33" [true]
       (run* [q]
             (emptyᵒ [])
             (≡ true q)))

(frame "2.34" [[]]
       (run* [x]
             (emptyᵒ x)))

(frame "2.38" []
       (run* [q]
             (eqᵒ 'pear 'plum)
             (≡ true q)))

(frame "2.39" [true]
       (run* [q]
             (eqᵒ 'plum 'plum)
             (≡ true q)))

(frame "2.52" [(list* #U0 #U1 'salad)]
       (run* [r]
             (fresh [x y]
                    (≡ r (list* x y 'salad)))))

(frame "2.54" [true]
       (run* [q]
             (pairᵒ (cons q q))
             (≡ true q)))

(frame "2.55" []
       (run* [q]
             (pairᵒ [])
             (≡ true q)))

(frame "2.56" []
       (run* [q]
             (pairᵒ 'pair)
             (≡ true q)))

(frame "2.57" [(list* #U0 #U1)]
       (run* [x]
             (pairᵒ x)))

(frame "2.58" [#U0]
       (run* [r]
             (pairᵒ (cons r 'pear))))
