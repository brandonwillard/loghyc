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
       (run* q
             (firstᵒ (cons :a (cons :c (cons :o (cons :r (cons :n nil))))) q)))

(frame "2.7" [true]
       (run* q
             (firstᵒ [:a :c :o :r :n] :a)
             (=ᵒ true q)))

(frame "2.8" [:pear]
       (run* q
             (fresh [x y]
                    (firstᵒ [q y] x)
                    (=ᵒ :pear x))))

(frame "2.11" [[:grape :a]]
       (run* q
             (fresh [x y]
                    (firstᵒ [:grape :raisin :pear] x)
                    (firstᵒ [[:a] [:b] [:c]] y)
                    (=ᵒ (cons x y) q))))

(frame "2.15" [:c]
       (run* q
             (fresh [v]
                    (restᵒ [:a :c :o :r :n] v)
                    (firstᵒ v q))))

;; 2.18 skipped, because of no cons in Hy

(frame "2.19" [true]
       (run* q
             (restᵒ [:a :c :o :r :n] [:c :o :r :n])
             (=ᵒ true q)))

(frame "2.20" [:o]
       (run* q
             (restᵒ [:c :o :r :n] [q :r :n])))

(frame "2.21" [[:a :c :o :r :n]]
       (run* q
             (fresh [x]
                    (restᵒ q [:c :o :r :n])
                    (firstᵒ q x)
                    (=ᵒ :a x))))

(frame "2.22" [[[:a :b :c] :d :e]]
       (run* q
             (consᵒ [:a :b :c] [:d :e] q)))

(frame "2.23" [:d]
       (run* q
             (consᵒ q [:a :b :c] [:d :a :b :c])))

(frame "2.24" [[:e :a :d :c]]
       (run* q
             (fresh [x y z]
                    (=ᵒ [:e :a :d x] q)
                    (consᵒ y [:a z :c] q))))

(frame "2.25" [:d]
       (run* q
             (consᵒ q [:a q :c] [:d :a q :c])))

(frame "2.26" [[:d :a :d :c]]
       (run* q
             (fresh [x]
                    (=ᵒ [:d :a x :c] q)
                    (consᵒ x [:a x :c] q))))

(frame "2.27" [[:d :a :d :c]]
       (run* q
             (fresh [x]
                    (consᵒ x [:a x :c] q)
                    (=ᵒ [:d :a x :c] q))))

(frame "2.29" [[:b :e :a :n :s]]
       (run* q
             (fresh [d x y w s]
                    (consᵒ w [:a :n :s] s)
                    (restᵒ q s)
                    (firstᵒ q x)
                    (=ᵒ :b x)
                    (restᵒ q d)
                    (firstᵒ d y)
                    (=ᵒ :e y))))
