;; adderall - miniKanren in Hy
;; Copyright (C) 2014  Gergely Nagy <algernon@madhouse-project.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(import [adderall.dsl [*]]
        [tests.schemer.common [*]])
(require adderall.dsl)
(require tests.schemer.common)

(frame "2.6" [:a]
       (run* [q]
             (firstᵒ (cons :a (cons :c (cons :o (cons :r (cons :n nil))))) q)))

(frame "2.7" [true]
       (run* [q]
             (firstᵒ [:a :c :o :r :n] :a)
             (≡ true q)))

(frame "2.8" [:pear]
       (run* [q]
             (fresh [x y]
                    (firstᵒ [q y] x)
                    (≡ :pear x))))

(frame "2.11" [[:grape :a]]
       (run* [q]
             (fresh [x y]
                    (firstᵒ [:grape :raisin :pear] x)
                    (firstᵒ [[:a] [:b] [:c]] y)
                    (≡ (cons x y) q))))

(frame "2.15" [:c]
       (run* [q]
             (fresh [v]
                    (restᵒ [:a :c :o :r :n] v)
                    (firstᵒ v q))))

(frame "2.18" [[[:raisin :pear] :a]]
       (run* [q]
             (fresh [x y]
                    (restᵒ [:grapre :raisin :pear] x)
                    (firstᵒ [[:a] [:b] [:c]] y)
                    (≡ (cons x y) q))))

(frame "2.19" [true]
       (run* [q]
             (restᵒ [:a :c :o :r :n] [:c :o :r :n])
             (≡ true q)))

(frame "2.20" [:o]
       (run* [q]
             (restᵒ [:c :o :r :n] [q :r :n])))

(frame "2.21" [[:a :c :o :r :n]]
       (run* [q]
             (fresh [x]
                    (restᵒ q [:c :o :r :n])
                    (firstᵒ q x)
                    (≡ :a x))))

(frame "2.22" [[[:a :b :c] :d :e]]
       (run* [q]
             (consᵒ [:a :b :c] [:d :e] q)))

(frame "2.23" [:d]
       (run* [q]
             (consᵒ q [:a :b :c] [:d :a :b :c])))

(frame "2.24" [[:e :a :d :c]]
       (run* [q]
             (fresh [x y z]
                    (≡ [:e :a :d x] q)
                    (consᵒ y [:a z :c] q))))

(frame "2.25" [:d]
       (run* [q]
             (consᵒ q [:a q :c] [:d :a q :c])))

(frame "2.26" [[:d :a :d :c]]
       (run* [q]
             (fresh [x]
                    (≡ [:d :a x :c] q)
                    (consᵒ x [:a x :c] q))))

(frame "2.27" [[:d :a :d :c]]
       (run* [q]
             (fresh [x]
                    (consᵒ x [:a x :c] q)
                    (≡ [:d :a x :c] q))))

(frame "2.29" [[:b :e :a :n :s]]
       (run* [q]
             (fresh [d x y w s]
                    (consᵒ w [:a :n :s] s)
                    (restᵒ q s)
                    (firstᵒ q x)
                    (≡ :b x)
                    (restᵒ q d)
                    (firstᵒ d y)
                    (≡ :e y))))

(frame "2.32" []
       (run* [q]
             (emptyᵒ [:grape :raisin :pear])
             (≡ true q)))

(frame "2.33" [true]
       (run* [q]
             (emptyᵒ [])
             (≡ true q)))

(frame "2.34" [[]]
       (run* [q]
             (emptyᵒ q)))

(frame "2.38" []
       (run* [q]
             (eqᵒ :pear :plum)
             (≡ true q)))

(frame "2.39" [true]
       (run* [q]
             (eqᵒ :plum :plum)
             (≡ true q)))

(frame "2.52" [(cons (unbound 0) (cons (unbound 1) :salad))]
       (run* [q]
             (fresh [x y]
                    (≡ q (cons x (cons y :salad))))))

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
             (pairᵒ :pair)
             (≡ true q)))

(frame "2.57" [(cons (unbound 0) (unbound 1))]
       (run* [q]
             (pairᵒ q)))

(frame "2.58" [(unbound 0)]
       (run* [q]
             (pairᵒ (cons q :pear))))
